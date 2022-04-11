Classes 'Table', 'ResultsElement', 'R6' 
<Table>
  Inherits from: <ResultsElement>
  Public:
  .createImages: function (...) 
    .footerForPrint: function () 
      .has: function (name) 
        .headerForPrint: function () 
          .render: function (image, ...) 
            .rowForPrint: function (i) 
              .setKey: function (key, index) 
                .setName: function (name) 
                  .setParent: function (parent) 
                    .titleForPrint: function () 
                      .update: function () 
                        .widthWidestCellInRow: function (row) 
                          .widthWidestHeader: function () 
                            addColumn: function (name, index = NA, title = name, superTitle = NULL, 
                                                 addFootnote: function (col, note, rowNo = NA, rowKey = NULL) 
                                                   addFormat: function (col, format, rowNo = NA, rowKey = NULL) 
                                                     addRow: function (rowKey, values = list()) 
                                                       addSymbol: function (col, symbol, rowNo = NA, rowKey = NULL) 
                                                         analysis: active binding
                                                 asDF: active binding
                                                 asProtoBuf: function (incAsText = FALSE, status = NULL) 
                                                   asString: function (.folded = FALSE) 
                                                     clone: function (deep = FALSE) 
                                                       columns: active binding
                                                 deleteRows: function () 
                                                   footnotes: active binding
                                                 fromProtoBuf: function (element, oChanges, vChanges) 
                                                   get: function (name) 
                                                     getBoundVars: function (expr) 
                                                       getCell: function (col, rowNo = NA, rowKey = NULL) 
                                                         getColumn: function (col) 
                                                           getRefs: function (recurse = FALSE) 
                                                             getRow: function (rowNo = NA, rowKey = NULL) 
                                                               getRows: function () 
                                                                 index: active binding
                                                 initialize: function (options, name = NULL, title = "no title", visible = TRUE, 
                                                                       isFilled: function (col, rowNo, rowKey, excHidden = TRUE) 
                                                                         isNotFilled: function () 
                                                                           key: active binding
                                                                       name: active binding
                                                                       names: active binding
                                                                       notes: active binding
                                                                       options: active binding
                                                                       parent: active binding
                                                                       path: active binding
                                                                       print: function () 
                                                                         requiresData: active binding
                                                                       resetVisible: function () 
                                                                         root: active binding
                                                                       rowCount: active binding
                                                                       rowKeys: active binding
                                                                       rowSelected: active binding
                                                                       saveAs: function (file, format) 
                                                                         setCell: function (col, value, rowNo = NA, rowKey = NULL) 
                                                                           setError: function (message) 
                                                                             setNote: function (key, note, init = TRUE) 
                                                                               setRefs: function (refs) 
                                                                                 setRow: function (values, rowNo = NA, rowKey = NULL) 
                                                                                   setSortKeys: function (col, keys) 
                                                                                     setState: function (state) 
                                                                                       setStatus: function (status) 
                                                                                         setTitle: function (title) 
                                                                                           setVisible: function (visible = TRUE) 
                                                                                             sortSelected: active binding
                                                                       state: active binding
                                                                       status: active binding
                                                                       title: active binding
                                                                       visible: active binding
                                                                       width: active binding
                                                                       Private:
                                                                         .clearWith: list
                                                                       .columns: list
                                                                       .error: NA
                                                                       .footnotes: NA
                                                                       .footnotesUpdated: FALSE
                                                                       .index: NA
                                                                       .key: NA
                                                                       .margin: 1
                                                                       .marstr:  
                                                                         .name: anova
                                                                       .notes: list
                                                                       .options: gamljGlmOptions, Options, R6
                                                                       .padding: 2
                                                                       .padstr:   
                                                                         .parent: Group, ResultsElement, R6
                                                                       .refs: 
                                                                         .rowCount: 0
                                                                       .rowKeys: list
                                                                       .rowNames: 
                                                                         .rowSelect: 
                                                                         .rowsExpr: 0
                                                                       .sortSelect: 
                                                                         .stale: FALSE
                                                                       .state: NULL
                                                                       .status: none
                                                                       .swapRowsColumns: FALSE
                                                                       .titleExpr: Simple Effects ANOVA of __key__
                                                                       .titleValue: Simple Effects ANOVA of __key__
                                                                       .updated: TRUE
                                                                       .updateFootnotes: function () 
                                                                         .visibleExpr: FALSE
                                                                       .visibleValue: FALSE
                                                                       deep_clone: function (name, value)  
                                                                         NULL
                                                                       