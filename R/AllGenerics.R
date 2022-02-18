#  MIT License
#
#  Copyright (c) 2017-2022 TileDB Inc.
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to deal
#  in the Software without restriction, including without limitation the rights
#  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in all
#  copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#  SOFTWARE.


#' Generic Methods
#'
#' Definition of generic methods
#'
#' @name generics
#'
#' @param object A TileDB object
#' @param value A value to be assigned
#' @param idx An index argument
#' @param ... Variable argument
NULL


#' @rdname generics
#' @param ... Currently unused
#' @export
setGeneric("schema", function(object, ...) standardGeneric("schema"))

#' @rdname generics
#' @param ... Currently unused
# ' @export
setGeneric("return.data.frame", function(object, ...) standardGeneric("return.data.frame"))

#' @rdname generics
# ' @export
setGeneric("return.data.frame<-", function(x, value) standardGeneric("return.data.frame<-"))

#' @rdname generics
#' @export
setGeneric("attrs<-", function(x, value) standardGeneric("attrs<-"))

## TODO: bring other generics here
#' @rdname generics
#' @export
setGeneric("raw_dump", function(object, ...) standardGeneric("raw_dump"))
