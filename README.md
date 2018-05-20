[![abaplint](https://abaplint.org/badges/larshp/ABAP-Object-Visualizer)](https://abaplint.org/project/larshp/ABAP-Object-Visualizer)

# ABAP-Object-Visualizer

Visualize complex object hierarchies in ABAP via [graphviz](http://www.graphviz.org/)

### Installation
Install via [abapGit](https://github.com/larshp/abapGit)

### Use
1. Set breakpoint in source code
2. In the new debugger go to "Script" tab
3. Load script ZRSTPDA_OBJECT_VISUALIZER from database
4. Choose option "Execute Directly"
5. Click "Start Script"
6. Enter object variable name
7. Paste clipboard to http://www.webgraphviz.com/ and click "Generate Graph!"

Graphviz can also be installed locally: http://portableapps.com/node/38245

Example from https://github.com/larshp/FORMfactor
![go_class](https://cloud.githubusercontent.com/assets/5888506/9976449/051927b2-5ee5-11e5-945d-4665ba704895.png)

Example from http://zevolving.com/2012/01/iterator-design-pattern-to-access-linked-list/
![linked](https://cloud.githubusercontent.com/assets/5888506/9976450/051bf514-5ee5-11e5-9b20-51b6a9d472b6.png)
