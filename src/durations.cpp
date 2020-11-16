#include <tiledb.h>

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
