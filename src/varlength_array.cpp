
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

// [[Rcpp::export]]
Rcpp::List firstTest(const std::string array_name,
                     const std::vector<int> subarray,
                     const std::vector<std::string> keys,
                     bool debug) {
  Context ctx;                                // context object
  Array array(ctx, array_name, TILEDB_READ);	// Prepare the array for reading

  if (subarray.size() != 4)
    Rcpp::stop("Expecting four elements in subarray vector.");
  if (keys.size() != 2)
    Rcpp::stop("Expecting two elements in keys vector.");

  ArraySchema schema = array.schema();
  tiledb_array_type_t array_type = schema.array_type();
  if (debug) std::cout << "Array is " << _tiledb_arraytype_to_string(array_type) << std::endl;

  Domain dom = schema.domain();

  uint32_t attr_num = schema.attribute_num();
  if (debug) std::cout << "Number of Attributes is " << attr_num << std::endl;
  for (uint32_t idx=0; idx<attr_num; idx++) {
    Attribute attr = schema.attribute(idx);
    if (debug) std::cout << "Name: " << attr.name()
                         << " " << _tiledb_datatype_to_string(attr.type()) << std::endl;
  }


  // Prepare the vectors that will hold the result
  auto max_el_map = array.max_buffer_elements(subarray);

  // std::vector<std::string> keys;
  // std::transform(std::begin(max_el_map), std::end(max_el_map), std::back_inserter(keys),
  //                [](std::decltype(max_el_map)::value_type const& pair) {
  //                  return pair.first;
  //                });
  std::string nm1(keys[0]), nm2(keys[1]);
  if (max_el_map.count(nm1) == 0)
    Rcpp::stop("Key '%s' not present.", nm1);
  if (max_el_map.count(nm2) == 0)
    Rcpp::stop("Key '%s' not present.", nm2);

  std::vector<uint64_t> a1_off(max_el_map[nm1].first);
  std::string a1_data;          						// TODO: generalize to data type from attr
  a1_data.resize(max_el_map[nm1].second);

  std::vector<uint64_t> a2_off(max_el_map[nm2].first);
  std::vector<int> a2_data(max_el_map[nm2].second); // TODO: ditto

  // Prepare and submit the query, and close the array
  Query query(ctx, array);
  query.set_subarray(subarray)
      .set_layout(TILEDB_ROW_MAJOR) 								// FIXME: does layout need to be a parameter?
      .set_buffer(nm1, a1_off, a1_data)
      .set_buffer(nm2, a2_off, a2_data);
  query.submit();
  array.close();


  // Get the string sizes
  auto result_el_map = query.result_buffer_elements();
  auto result_el_a1_off = result_el_map[nm1].first;
  std::vector<uint64_t> a1_str_sizes;
  for (size_t i = 0; i < result_el_a1_off - 1; ++i)
    a1_str_sizes.push_back(a1_off[i + 1] - a1_off[i]);
  auto result_a1_data_size = result_el_map[nm1].second * sizeof(char);
  a1_str_sizes.push_back(result_a1_data_size - a1_off[result_el_a1_off - 1]);

  // Get the strings
  std::vector<std::string> a1_str;
  for (size_t i = 0; i < result_el_a1_off; ++i)
    a1_str.push_back(std::string(&a1_data[a1_off[i]], a1_str_sizes[i]));

  // Get the element offsets
  std::vector<uint64_t> a2_el_off;
  auto result_el_a2_off = result_el_map[nm2].first;
  for (size_t i = 0; i < result_el_a2_off; ++i)
    a2_el_off.push_back(a2_off[i] / sizeof(int));

  // Get the number of elements per cell value
  std::vector<uint64_t> a2_cell_el;
  for (size_t i = 0; i < result_el_a2_off - 1; ++i)
    a2_cell_el.push_back(a2_el_off[i + 1] - a2_el_off[i]);
  auto result_el_a2_data = result_el_map["a2"].second;
  a2_cell_el.push_back(result_el_a2_data - a2_el_off.back());

  int nr = subarray[1] - subarray[0] + 1;
  int nc = subarray[3] - subarray[2] + 1;
  Rcpp::CharacterMatrix A1(nr, nc);
  Rcpp::List A2l(nr * nc);

  // Print the results
  for (size_t i = 0; i < result_el_a1_off; ++i) {
    if (debug) {
      Rcpp::Rcout << "i: " << i << " ";
      Rcpp::Rcout << "a1: " << a1_str[i] << ", a2: ";
    }
    std::vector<int> v;
    for (size_t j = 0; j < a2_cell_el[i]; ++j) {
      if (debug) Rcpp::Rcout << a2_data[a2_el_off[i] + j] << " ";
      v.push_back(a2_data[a2_el_off[i] + j] );
    }
    if (debug) Rcpp::Rcout << "\n";
    A1[i] = a1_str[i];
    A2l[i] = v;
  }

  return Rcpp::List::create(Rcpp::Named(nm1) = transpose(A1),
                            Rcpp::Named(nm2) = A2l);

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
