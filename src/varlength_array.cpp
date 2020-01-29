
#include <tiledb.h>

#define STRICT_R_HEADERS
#include <Rcpp.h>

using namespace tiledb;

const char* _tiledb_datatype_to_string(tiledb_datatype_t dtype);

const char* _tiledb_arraytype_to_string(tiledb_array_type_t atype) {
  switch (atype) {
    case TILEDB_DENSE:
      return "dense";
    case TILEDB_SPARSE:
      return "sparse";
    default:
      Rcpp::stop("unknow tiledb_array_type_t");
  }
}

const size_t _tiledb_datatype_sizeof(const tiledb_datatype_t dtype) {
  switch(dtype) {
    case TILEDB_INT32:
      return sizeof(int32_t);
    case TILEDB_CHAR:
      return sizeof(char);
    default:
      Rcpp::stop("Unsupported tiledb_datatype_t '%s'", _tiledb_datatype_to_string(dtype));
  }
}

// We need to 'cache' the variable length data and offset vectors as
// these get passed to the storage manager layer all at once.  But we
// can only process them one by one, and only know respective types
// when we process. Which is generally in some dynamic scope so we
// would 'forgot' the information. Hence we fill a simple structure
// and use it to set the query.
//
// This is currently used only on the writing side

struct vararrelem {
  std::string attr;             // attribute name
  tiledb_datatype_t dtype;      // attribute type
  uint64_t *offsets;            // pointer to offset values
  uint64_t noffsets;            // number of offset values
  void *data;                   // pointer to data values
  uint64_t ndata;               // number of data values
  int32_t elsize;               // sizeof(T) for the attribute
};


// [[Rcpp::export]]
Rcpp::List read_varlength_array(const std::string array_name,
                                const std::string key,
                                const std::vector<int> subarray,
                                bool debug) {
  Context ctx;                                // context object
  Array array(ctx, array_name, TILEDB_READ);	// Prepare the array for reading

  if (subarray.size() != 4)
    Rcpp::stop("Expecting four elements in subarray vector.");

  //int nkeys = keys.size();
  //if (keys.size() != 2) Rcpp::stop("Expecting two elements in keys vector.");

  ArraySchema schema = array.schema();
  tiledb_array_type_t array_type = schema.array_type();
  if (debug) Rcpp::Rcout << "Array is " << _tiledb_arraytype_to_string(array_type) << " " << array_type << std::endl;

  Domain dom = schema.domain();

  // attribute by key, errors appropriately if key not present
  Attribute attr = schema.attribute(key);
  tiledb_datatype_t dtype = attr.type();

  // Prepare the vectors that will hold the result
  auto max_el_map = array.max_buffer_elements(subarray);

  std::vector<uint64_t> offsets(max_el_map[key].first);
  char* data = new char[max_el_map[key].second * _tiledb_datatype_sizeof(dtype)];

  // Prepare and submit the query, and close the array
  Query query(ctx, array);
  query.set_subarray(subarray);
  query.set_layout(TILEDB_ROW_MAJOR); 								// FIXME: does layout need to be a parameter?
  query.set_buffer(key, offsets.data(), offsets.size(),
                   static_cast<void*>(data), max_el_map[key].second * _tiledb_datatype_sizeof(dtype));

  query.submit();
  array.close();

  auto result_el_map = query.result_buffer_elements();
  if (debug) Rcpp::Rcout << "result_el_map after "
                         << result_el_map[key].first << " "
                         << result_el_map[key].second
                         << std::endl;

  int nr = subarray[1] - subarray[0] + 1;
  int nc = subarray[3] - subarray[2] + 1;
  Rcpp::List reslist(nr * nc);

  if (dtype == TILEDB_CHAR) {

    auto result_el_off = result_el_map[key].first;
    std::vector<uint64_t> str_sizes;
    for (size_t i = 0; i < result_el_off - 1; ++i)
      str_sizes.push_back(offsets[i + 1] - offsets[i]);
    auto result_data_size = result_el_map[key].second * sizeof(char);
    str_sizes.push_back(result_data_size - offsets[result_el_off - 1]);

    // Get the strings
    std::vector<std::string> str;
    for (size_t i = 0; i < result_el_off; ++i) {
      std::string txt = std::string(&data[offsets[i]], str_sizes[i]);
      str.push_back(txt);
      reslist[i] = txt;
    }

    if (debug) for (size_t i=0; i<str.size(); i++) Rcpp::Rcout << "  " << str[i] << std::endl;

  } else if (dtype == TILEDB_INT32) {

    // Allocate and fill vector of dtype
    size_t ndata = result_el_map[key].second;
    std::vector<int32_t> vdata(ndata);
    memcpy(vdata.data(), data, ndata*sizeof(int32_t));
    // GOOD! for (int i=0; i<vdata.size(); i++) std::cout << vdata[i] << "\n";

    // Get the element offsets
    std::vector<uint64_t> el_off;
    auto result_el_off = result_el_map[key].first;
    for (size_t i = 0; i < result_el_off; ++i) {
      el_off.push_back(offsets[i] / sizeof(int32_t));
      //std::cout << "Pushing back " << offsets[i] << " " << offsets[i]/sizeof(int) << std::endl;
    }

    // Get the number of elements per cell value
    std::vector<uint64_t> cell_el;
    for (size_t i = 0; i < result_el_off - 1; ++i) {
      cell_el.push_back(el_off[i + 1] - el_off[i]);
      //std::cout << "Counts per cell " << el_off[i + 1] - el_off[i] << std::endl;
    }
    auto result_el_data = result_el_map[key].second;
    cell_el.push_back(result_el_data - el_off.back());

    // Print the results
    if (debug) {
      for (size_t i = 0; i < result_el_off; ++i) {
        Rcpp::Rcout << "  ";
        for (size_t j = 0; j < cell_el[i]; ++j)
          Rcpp::Rcout << vdata[el_off[i] + j] << " ";
        Rcpp::Rcout << "\n";
      }
    }
    for (size_t i = 0; i < result_el_off; ++i) {
      // for each cell, fill a vector 'v'
      std::vector<int32_t> v;
      for (size_t j = 0; j < cell_el[i]; ++j)
        v.push_back(vdata[el_off[i] + j]);
      reslist[i] = v;
    }
  }
  delete[] data;
  return reslist;
}

// edd@rob:~/git/tiledb-adhoc/python(master)$ python3 variable_length.py
// [['a' 'bb' 'ccc' 'dd']
//  ['eee' 'f' 'g' 'hhh']
//  ['i' 'jjj' 'kk' 'l']
//  ['m' 'n' 'oo' 'p']]
// [[array([1]) array([2, 2]) array([3]) array([4])]
//  [array([5]) array([6, 6]) array([7, 7]) array([8, 8, 8])]
//  [array([9, 9]) array([10]) array([11]) array([12, 12])]
//  [array([13]) array([14, 14, 14]) array([15]
// edd@rob:~/git/tiledb-adhoc/python(master)$

// and now from R:

// edd@rob:~/git/tiledb-r(de/varlength-array)$ r -ltiledb  -e'print(variable_length("../tiledb-data/examples/variable_length_array", c(1,4,1,4), c("a1", "a2"))); '
// $a1
//     V1  V1  V1  V1
// 1:   a  bb ccc  dd
// 2: eee   f   g hhh
// 3:   i jjj  kk   l
// 4:   m   n  oo   p
//
// $a2
//     V1       V1  V1    V1
// 1: 1,1      2,2   3     4
// 2:   5      6,6 7,7 8,8,8
// 3: 9,9       10  11 12,12
// 4:  13 14,14,14  15    16
//
// edd@rob:~/git/tiledb-r(de/varlength-array)$



std::pair<std::string, std::vector<uint64_t>> getStringVectorAndOffset(Rcpp::DataFrame df,
                                                                       bool debug = FALSE) {
  // here we know we have a data.frame with character columns
  int k = df.length();
  Rcpp::List fst = df[0];
  int n = fst.length();
  if (debug) Rcpp::Rcout << "  with " << k << " columns and " << n << " elements yielding ";

  std::string data("");
  std::vector<uint64_t> offsets;
  uint64_t curroff = 0;
  offsets.push_back(curroff);          // offsets start with 0

  for (int i=0; i<n; i++) {
    for (int j=0; j<k; j++) {
      Rcpp::List cvec = df[j];
      std::string curstr = Rcpp::as<std::string>(cvec[i]);
      data += curstr;
      curroff += curstr.size();
      offsets.push_back(curroff);
    }
  }
  offsets.pop_back(); // last one is 'one too far'
  return std::make_pair(data, offsets);
}

template <typename T>
std::pair<std::vector<T>, std::vector<uint64_t>> getVectorAndOffset(Rcpp::DataFrame df,
                                                                    bool debug = FALSE) {
  // here we know we have a data.frame with T elements (int or real)
  int ncolumns = df.length();
  Rcpp::List fst = df[0];
  int nrows = fst.length();
  if (debug) Rcpp::Rcout << "  with " << ncolumns << " columns and " << nrows << " elements yielding ";

  std::vector<T> data;
  std::vector<uint64_t> offset_els;
  uint64_t curroff = 0;
  offset_els.push_back(curroff);          // offsets start with 0

  for (int i=0; i<nrows; i++) {
    for (int j=0; j<ncolumns; j++) {
      Rcpp::List cvec = df[j];
      std::vector<T> curvec = Rcpp::as<std::vector<T> >(cvec[i]);
      for (size_t vi=0; vi<curvec.size(); vi++) {
        data.push_back(curvec[vi]);
        if (debug) Rcpp::Rcout << " " << curvec[vi];
      }
      curroff += curvec.size();
      offset_els.push_back(curroff);
    }
  }
  if (debug) Rcpp::Rcout << std::endl;
  offset_els.pop_back(); // last one is 'one too far'

  std::vector<uint64_t> offsets;
  for (auto e : offset_els) {
    offsets.push_back(e * sizeof(T));
  }
  return std::make_pair(data, offsets);
}

// [[Rcpp::export]]
void create_varlength_array(const std::string array_name) {
  // Create a TileDB context
  Context ctx;

  // The array will be 4x4 with dimensions "rows" and "cols", with domain [1,4]
  Domain domain(ctx);
  domain.add_dimension(tiledb::Dimension::create<int>(ctx, "rows", {{1, 4}}, 4))
      .add_dimension(tiledb::Dimension::create<int>(ctx, "cols", {{1, 4}}, 4));

  // The array will be dense
  ArraySchema schema(ctx, TILEDB_DENSE);
  schema.set_domain(domain).set_order({{TILEDB_ROW_MAJOR, TILEDB_ROW_MAJOR}});

  // Add two variable-length attributes "a1" and "a2", the first storing
  // strings and the second storing a variable number of integers.
  schema.add_attribute(Attribute::create<std::string>(ctx, "a1"));
  schema.add_attribute(Attribute::create<std::vector<int>>(ctx, "a2"));

  // Create the (empty) array on disk.
  Array::create(array_name, schema);
}

// [[Rcpp::export]]
bool write_varlength_array(const std::string uri, Rcpp::List listobject,
                           const std::vector<std::string> names, bool debug = false) {
  int n = names.size();
  if (debug) Rcpp::Rcout << "n is " << n << std::endl;
  if (debug) Rcpp::print(listobject);

  Context ctx;                                // context object
  Array array(ctx, uri, TILEDB_WRITE);	      // Prepare the array for writing
  Query query(ctx, array);
  query.set_layout(TILEDB_ROW_MAJOR);

  std::vector<struct vararrelem> vec;

  // simplest possible processing: assign to data frame
  for (int i=0; i<n; i++) {
    if (debug) Rcpp::Rcout << "Object " << i << " with name " << names[i] << std::endl;
    Rcpp::DataFrame df(listobject[i]);
    int k = df.length();
    Rcpp::List fst = df[0];
    RObject obj = fst[0];

    switch(TYPEOF(obj)) {
      case VECSXP: {
        Rcpp::stop("List objects are not supported.");
        break;// not reached
      }
      case REALSXP: {
        if (debug) Rcpp::Rcout << "double\n";
        std::pair<std::vector<double>, std::vector<uint64_t>> vv = getVectorAndOffset<double>(df, debug);

        struct vararrelem s;
        s.attr = names[i];
        s.noffsets = vv.second.size();
        s.offsets = new uint64_t[s.noffsets];
        memcpy(s.offsets, vv.second.data(), s.noffsets*sizeof(uint64_t));
        s.ndata = vv.first.size();
        s.elsize = sizeof(double);
        s.data = new char[s.ndata*s.elsize];
        memcpy(s.data, vv.first.data(), s.ndata*s.elsize);
        vec.push_back(s);

        break;
      }
      case INTSXP: {
        if (debug) Rcpp::Rcout << "integer\n";
        std::pair<std::vector<int32_t>, std::vector<uint64_t>> vv = getVectorAndOffset<int32_t>(df, debug);

        struct vararrelem s;
        s.attr = names[i];
        s.noffsets = vv.second.size();
        s.offsets = new uint64_t[s.noffsets];
        memcpy(s.offsets, vv.second.data(), s.noffsets*sizeof(uint64_t));
        s.ndata = vv.first.size();
        s.elsize = sizeof(int32_t);
        s.data = new char[s.ndata*s.elsize];
        memcpy(s.data, vv.first.data(), s.ndata*s.elsize);
        vec.push_back(s);

        break;
      }
      case STRSXP: {
        if (debug) Rcpp::Rcout << "character\n";
        std::pair<std::string, std::vector<uint64_t>> vv = getStringVectorAndOffset(df, debug);
        //if (debug) Rcpp::Rcout << vv.first << std::endl;

        struct vararrelem s;
        s.attr = names[i];
        s.noffsets = vv.second.size();
        s.offsets = new uint64_t[s.noffsets];
        memcpy(s.offsets, vv.second.data(), s.noffsets*sizeof(uint64_t));
        s.ndata = vv.first.size();
        s.elsize = sizeof(char);
        s.data = new char[s.ndata];
        memcpy(s.data, &vv.first[0], s.ndata*s.elsize);
        vec.push_back(s);

        break;
      }
    }
    for (int j=0; j<k; j++) {
      Rcpp::List s = df[j];
      if (debug) Rcpp::print(s[0]);
    }
    //Rcpp::Rcout << df[0][0] << std::endl;

    if (debug) Rcpp::Rcout << "DONE one pass on " << names[i] << std::endl;
  }

  // Now use the 'cached' data to set the buffer
  for (size_t i=0; i<vec.size(); i++) {
    struct vararrelem s = vec[i];
    if (debug) Rcpp::Rcout << "Data i " << i << "  noff "
                           << s.noffsets << "  nd " << s.ndata << "  sz "
                           << s.elsize << std::endl;
    query.set_buffer(s.attr, s.offsets, s.noffsets, s.data, s.ndata);
  }

  // Perform the write and close the array.
  query.submit();
  array.close();

  // Release temp 'cache' memory
  for (size_t i=0; i<vec.size(); i++) {
    struct vararrelem s = vec[i];
    delete[] s.offsets;
    delete[] static_cast<char*>(s.data);
  }

  return true;
}
