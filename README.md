# ABAP-Object-Visualizer

Visualize complex object hierachies in ABAP via [graphviz](http://www.graphviz.org/)

### Limitations
Does not work if the data contains cyclic references

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

Alternatively graphviz can be installed locally: http://portableapps.com/node/38245
