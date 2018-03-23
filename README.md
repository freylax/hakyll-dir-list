# hakyll-dir-list
[Hakyll](https://jaspervdj.be/hakyll/)
extension which supports the creation of hierarchical menus from source files in a directory structure.
Adittionally single page web sites can created by using flattened page id's instead of urls.

## Build the library
* `stack build`

## Usage

The exported `dirListField` function is similar to the Hakyll [`listField`](https://jaspervdj.be/hakyll/reference/Hakyll-Web-Template-Context.html#v:listField)
template function but creates additional context information which can be used in the template to create a hierarchical menu.
### Context usable inside the template 
* `$begin-tags$` injects `<li>` and `<ul>` tags if apropriate
* `$end-tags$` contains the corresponding `</li>` and `</ul>` tags
* `$full-page-id$` is the hyphen seperated path of the page
* use configuration to get level dependant structuring tags 
### Metainformation in the source files
For each subdirectory which should be processed one source file with the same base name should
exist which can contain meta information:
* `pages/a.md`       top page for directory a
* `pages/a/foo.md`   page foo within a


The following meta information can be given
* `page-id`          part of the generated id, if not given the base name of the file
* `page-order`       give an ordering key for sorting in the current directory level, if not given the `page-id` will be used

## Example
See the example for an illustration of usage.


       

