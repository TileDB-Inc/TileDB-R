#pragma once

// simplified / minimal version of spdl.h not exposing the fmt::format interface

#include <string>

// includes the auto-generated exports for the exported (via a C interface) underlying C++ functions
#include <tinyspdl/RcppSpdlog.h>

namespace std {
    inline std::string to_string(const char s[]) { return std::string(s); }
    inline std::string to_string(std::string &s) { return std::string(s); }
}

// for convenience define cuter ones in another (shorter) namespace
namespace spdl {
    inline void setup(const std::string& name = "default",
                      const std::string& level = "warn") {  RcppSpdlog::log_setup(name, level); }
    inline void filesetup(const std::string& filename = "default",
                          const std::string& name = "default",
                          const std::string& level = "warn") {  RcppSpdlog::log_filesetup(name, level); }
    inline void drop(const std::string& name) { RcppSpdlog::log_drop(name); }
    inline void set_pattern(const std::string& s) { RcppSpdlog::log_set_pattern(s); }
    inline void set_level(const std::string& s) { RcppSpdlog::log_set_level(s); }
    inline void trace(const std::string& s) { RcppSpdlog::log_trace(s); }
    inline void debug(const std::string& s) { RcppSpdlog::log_debug(s); }
    inline void info(const std::string& s) { RcppSpdlog::log_info(s); }
    inline void warn(const std::string& s) { RcppSpdlog::log_warn(s); }
    inline void error(const std::string& s) { RcppSpdlog::log_error(s); }
    inline void critical(const std::string& s) { RcppSpdlog::log_critical(s); }

    // it is highly unlikely we find a package imposing C++98 as R itself now defaults to C++14
    // and many packages have opted into C++11 (or newer) but the check does not hurt
    #if __cplusplus >= 201103L

    // collation helper turn all variadic arguments into strings so that a string string can be passed to RcppSpdlog::log_*(s) functions
    inline std::string toString()                        { return std::string(); }
    inline std::string toString(const char s[])          { return std::string(s); }
    inline std::string toString(const bool b)            { return std::string(b ? "true" : "false"); }
    template <typename T, typename ... ARGS>
    inline std::string toString(T&& t, ARGS&& ... args)  { return std::to_string(t) + " " + toString(args...);}

    inline std::string collate(const std::string s) { return s; }

    template <typename ... TT>
    inline std::string collate(const std::string s, TT&& ... tt) {
        std::string res{s + toString(std::forward<TT>(tt)...)};
        return res;
    }

    template <typename... Args>
    inline void trace(const char* fmt, Args&&... args ) { RcppSpdlog::log_trace(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    template <typename... Args>
    inline void debug(const char* fmt, Args&&... args ) { RcppSpdlog::log_debug(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    template <typename... Args>
    inline void info(const char* fmt, Args&&... args ) { RcppSpdlog::log_info(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    template <typename... Args>
    inline void warn(const char* fmt, Args&&... args ) { RcppSpdlog::log_warn(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    template <typename... Args>
    inline void error(const char* fmt, Args&&... args ) { RcppSpdlog::log_error(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    template <typename... Args>
    inline void critical(const char* fmt, Args&&... args ) { RcppSpdlog::log_critical(collate(fmt, std::forward<Args>(args)... ).c_str()); }

    #endif // if C++11

    //inline Rcpp::XPtr<spdlog::stopwatch> stopwatch() { return RcppSpdlog::get_stopwatch(); }
    //inline double elapsed(Rcpp::XPtr<spdlog::stopwatch> w) { return RcppSpdlog::elapsed_stopwatch(w); }
    //inline std::string format(Rcpp::XPtr<spdlog::stopwatch> w) { return RcppSpdlog::format_stopwatch(w); }

}
