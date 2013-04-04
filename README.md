

[![Build Status](https://travis-ci.org/zkessin/json_patch.png)](https://travis-ci.org/zkessin/json_patch)

This module exports 1 function patch/2 which takes a JSON containing
the patch instructions as well as the json to change. Both can be
passed in as a binary json or as decoded by the jsx:decode/1 function. If
you pass in the data is a json it will return it in the same form.

The JSON Patch Protocol can be seen here: 
http://tools.ietf.org/id/draft-ietf-appsawg-json-patch-10.txt


Code is copyright 2013 by Product Structure
Can be used under the BSD/3 license. 

Please let us know if you find it useful or have issues.
 