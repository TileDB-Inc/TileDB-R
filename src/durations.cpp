#include <tiledb.h>

Rcpp::NumericVector makeNanotime(const std::vector<int64_t>& vec); // in utilities.cpp

std::vector<int64_t> dates_to_int64(Rcpp::DateVector dv, tiledb_datatype_t dtype) {
  size_t n = dv.size();
  std::vector<int64_t> iv(n);
  for (size_t i=0; i<n; i++) {
    Rcpp::Date dt(dv[i]);
    switch (dtype) {
    case TILEDB_DATETIME_YEAR:
      iv[i] = static_cast<int64_t>(dt.getYear() - 1970);
      break;
    case TILEDB_DATETIME_MONTH:
      iv[i] = static_cast<int64_t>(12*(dt.getYear() - 1970) + dt.getMonth() - 1);
      break;
    case TILEDB_DATETIME_WEEK:
      // Hinnant's Date library can do weeks but it appears to just equal days/7
      iv[i] = static_cast<int64_t>(dt.getDate()) / 7;
      break;
    case TILEDB_DATETIME_DAY:
      iv[i] = static_cast<int64_t>(dt.getDate());
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for Date to int64 conversion", dtype);
    }
  }
  return iv;
}

Rcpp::DateVector int64_to_dates(std::vector<int64_t> iv, tiledb_datatype_t dtype) {
  int n = iv.size();
  Rcpp::DateVector dv(n);
  for (int i=0; i<n; i++) {
    switch (dtype) {
    case TILEDB_DATETIME_YEAR:
      dv[i] = Date(iv[i] + 1970, 1, 1);
      break;
    case TILEDB_DATETIME_MONTH:
      dv[i] = Date(iv[i] / 12 + 1970, iv[i] % 12 + 1, 1);
      break;
    case TILEDB_DATETIME_WEEK:
      // Hinnant's Date library can do weeks but it appears to just equal days/7
      dv[i] = Date(static_cast<int>(iv[i] + 1) * 7 - 1);
      break;
    case TILEDB_DATETIME_DAY:
      dv[i] = Date(static_cast<int>(iv[i]));
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for int64 to Date conversion", dtype);
    }
  }
  return dv;
}

std::vector<int64_t> datetimes_to_int64(Rcpp::DatetimeVector dv, tiledb_datatype_t dtype) {
  size_t n = dv.size();
  std::vector<int64_t> iv(n);
  for (size_t i=0; i<n; i++) {
    Rcpp::Datetime dt(dv[i]);
    switch (dtype) {
    case TILEDB_DATETIME_HR:
      iv[i] = static_cast<int64_t>(double(dt)/3600);
      break;
    case TILEDB_DATETIME_MIN:
      iv[i] = static_cast<int64_t>(double(dt)/60);
      break;
    case TILEDB_DATETIME_SEC:
      // Hinnant's Date library can do weeks but it appears to just equal days/7
      iv[i] = static_cast<int64_t>(double(dt));
      break;
    case TILEDB_DATETIME_MS:
      iv[i] = static_cast<int64_t>(double(dt) * 1e3);
      break;
    case TILEDB_DATETIME_US:
      iv[i] = static_cast<int64_t>(double(dt) * 1e6);
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for Datetime to int64 conversion", dtype);
    }
  }
  return iv;
}

Rcpp::DatetimeVector int64_to_datetimes(std::vector<int64_t> iv, tiledb_datatype_t dtype) {
  int n = iv.size();
  Rcpp::DatetimeVector dv(n);
  for (int i=0; i<n; i++) {
    switch (dtype) {
    case TILEDB_DATETIME_HR:
      dv[i] = iv[i] * 3600;
      break;
    case TILEDB_DATETIME_MIN:
      dv[i] = iv[i] * 60;
      break;
    case TILEDB_DATETIME_SEC:
      dv[i] = iv[i];
      break;
    case TILEDB_DATETIME_MS:
      dv[i] = iv[i] * 1e-3;
      break;
    case TILEDB_DATETIME_US:
      dv[i] = iv[i] * 1e-6;
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for int64 to Datetime conversion", dtype);
    }
  }
  return dv;
}

std::vector<int64_t> subnano_to_int64(NumericVector nv, tiledb_datatype_t dtype) {
  size_t n = nv.size();
  std::vector<int64_t> iv(n);
  memcpy(iv.data(), &(nv[0]), n*sizeof(double));
  for (size_t i=0; i<n; i++) {
    switch (dtype) {
    case TILEDB_DATETIME_NS:
      // nothing to do for int64 as nanotime is already the basecase
      break;
    case TILEDB_DATETIME_PS:
      iv[i] = iv[i] * 1e3;
      break;
    case TILEDB_DATETIME_FS:
      iv[i] = iv[i] * 1e6;
      break;
    case TILEDB_DATETIME_AS:
      iv[i] = iv[i] * 1e9;
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for subnano to int64 conversion", dtype);
    }
  }
  return iv;
}

Rcpp::NumericVector int64_to_subnano(std::vector<int64_t> iv, tiledb_datatype_t dtype) {
  int n = iv.size();
  for (int i=0; i<n; i++) {
    switch (dtype) {
    case TILEDB_DATETIME_NS:
      // nothing to do for int64 as nanotime is already the basecase
      break;
    case TILEDB_DATETIME_PS:
      iv[i] = iv[i] / 1e3;
      break;
    case TILEDB_DATETIME_FS:
      iv[i] = iv[i] / 1e6;
      break;
    case TILEDB_DATETIME_AS:
      iv[i] = iv[i] / 1e9;
      break;
    default:
      Rcpp::stop("Inapplicable conversion tiledb_datatype_t (%d) for int64 to subnano conversion", dtype);
    }
  }
  return makeNanotime(iv);
}

// Check whether a column datatype is date or datetime
//
bool is_datetime_column(const tiledb_datatype_t dtype) {
  return (dtype == TILEDB_DATETIME_YEAR)
    || (dtype == TILEDB_DATETIME_MONTH)
    || (dtype == TILEDB_DATETIME_WEEK)
    || (dtype == TILEDB_DATETIME_DAY)
    || (dtype == TILEDB_DATETIME_HR)
    || (dtype == TILEDB_DATETIME_MIN)
    || (dtype == TILEDB_DATETIME_SEC)
    || (dtype == TILEDB_DATETIME_MS)
    || (dtype == TILEDB_DATETIME_US)
    || (dtype == TILEDB_DATETIME_NS)
    || (dtype == TILEDB_DATETIME_PS)
    || (dtype == TILEDB_DATETIME_FS)
    || (dtype == TILEDB_DATETIME_AS)
    ;
}
