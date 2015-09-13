# ABAP-Object-Visualizer

Visualize complex object hierarchies in ABAP via [graphviz](http://www.graphviz.org/)

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

Example from https://github.com/larshp/FORMfactor
![image](https://cloud.githubusercontent.com/assets/5888506/9831830/9992cd50-5966-11e5-9ee5-383ad590a683.png)

Example from http://zevolving.com/2012/01/iterator-design-pattern-to-access-linked-list/
![image](https://cloud.githubusercontent.com/assets/5888506/9835610/b9f6f188-59f3-11e5-8da6-d62e7cf4d4fe.png)
