var rtermFormat = require('./rtermFormat');

const events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
        updateSimpleSupplier(ui, this);
        updateRandomSupplier(ui,this);

    },

    onChange_factors: function(ui) {
        calcModelTerms(ui, this);
        updateRandomSupplier(ui,this);
 
    },

    onChange_covariates: function(ui) {
        calcModelTerms(ui, this);
        updateRandomSupplier(ui,this);

    },
    onChange_cluster: function(ui) {
        updateRandomSupplier(ui,this);

    },

    onChange_randomSupplier: function(ui){
        let supplierList = this.itemsToValues(ui.randomSupplier.value());
        console.log("change in random supplier");
        var changes = this.findChanges("randomSupplier",supplierList,rtermFormat);
        if (changes.removed.length>0) {
          var randomTerms = this.cloneArray(ui.randomTerms.value(),[]);
          var  light = removeFromMultiList(changes.removed,randomTerms,this,1);
          ui.randomTerms.setValue(light);
        }
        console.log(changes);
//        this.checkValue(ui.randomTerms, true, supplierList, rtermFormat);
        return;
    },
    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
        updateSimpleSupplier(ui, this);
        updateRandomSupplier(ui,this);

    },

    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plotHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepPlots, false, values, FormatDef.variable);
    },
    
        onChange_simpleSupplier: function(ui) {
        let values = this.itemsToValues(ui.simpleSupplier.value());
        this.checkValue(ui.simpleVariable, false, values, FormatDef.variable);
        this.checkValue(ui.simpleModerator, false, values, FormatDef.variable);
        this.checkValue(ui.simple3way, false, values, FormatDef.variable);
    },

 
    onChange_postHocSupplier: function(ui) {
        let values = this.itemsToValues(ui.postHocSupplier.value());
        this.checkValue(ui.postHoc, true, values, FormatDef.term);
    },
    onChange_randomTerms: function(ui) {
 //       randomTerms(ui,this);
 //       filterRandomTerms(ui, this);
    },
    onEvent_randomTerms_preprocess: function(ui, data) {
 //       for(var j = 0; j < data.items.length; j++) {
//          data.items[j].value.raw=data.items[j].value.toString();
//      }
    },
        onEvent_nothing: function(ui, data) {
          console.log("I didn't do anything");
    }    

};

var calcModelTerms = function(ui, context) {
    var variableList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var combinedList = variableList.concat(covariatesList);
    ui.modelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.simpleSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
 
    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);


    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var termsChanged = false;

    for (var i = 0; i < combinedDiff.removed.length; i++) {
        for (var j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], combinedDiff.removed[i])) {
                termsList.splice(j, 1);
                termsChanged = true;
                j -= 1;
            }
        }
    }


    for (var a = 0; a < diff.added.length; a++) {
        let item = diff.added[a];
        var listLength = termsList.length;
        for (var j = 0; j < listLength; j++) {
            var newTerm = context.clone(termsList[j]);
            if (containsCovariate(newTerm, covariatesList) === false) {
                if (context.listContains(newTerm, item, FormatDef.variable) === false) {
                    newTerm.push(item)
                    if (context.listContains(termsList, newTerm , FormatDef.term) === false) {
                        termsList.push(newTerm);
                        termsChanged = true;
                    }
                }
            }
        }
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    for (var a = 0; a < diff2.added.length; a++) {
        let item = diff2.added[a];
        if (context.listContains(termsList, [item] , FormatDef.term) === false) {
            termsList.push([item]);
            termsChanged = true;
        }
    }

    if (termsChanged)
        ui.modelTerms.setValue(termsList);

    updateContrasts(ui, variableList, context);
    updateScaling(ui, covariatesList, context);
};

var updateSimpleSupplier = function(ui, context) {
        var termsList = context.cloneArray(ui.modelTerms.value(), []);
        var varList=[];
        for (var j = 0; j < termsList.length; j++) {
            var newTerm=context.clone(termsList[j]);
            if (newTerm.length==1) {
                  varList.push(newTerm[0]); // was varList.push(newTerm);
            }
        }
        varList=context.valuesToItems(varList, FormatDef.variable);
        ui.simpleSupplier.setValue(varList);
        ui.plotsSupplier.setValue(varList);

    };



var updatePostHocSupplier = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];
        if (containsCovariate(term, covariatesList) === false)
            list.push(term);
    }
    ui.postHocSupplier.setValue(context.valuesToItems(list, FormatDef.term));
};

var filterModelTerms = function(ui, context) {
    var termsList = context.cloneArray(ui.modelTerms.value(), []);
    var diff = context.findChanges("termsList", termsList, true, FormatDef.term);

    var changed = false;
    if (diff.removed.length > 0) {
        var itemsRemoved = false;
        for (var i = 0; i < diff.removed.length; i++) {
            var item = diff.removed[i];
            for (var j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], item)) {
                    termsList.splice(j, 1);
                    j -= 1;
                    itemsRemoved = true;
                }
            }
        }

        if (itemsRemoved)
            changed = true;
    }

    if (context.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(termsList);
};

var updateContrasts = function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);

    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "simple" });
        else
            list3.push(found);
    }

    ui.contrasts.setValue(list3);
};

var updateScaling = function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.scaling.value(), []);

    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "centered" });
        else
            list3.push(found);
    }

    ui.scaling.setValue(list3);
};

var containsCovariate = function(value, covariates) {
    for (var i = 0; i < covariates.length; i++) {
        if (FormatDef.term.contains(value, covariates[i]))
            return true;
    }

    return false;
};


var updateRandomSupplier = function(ui, context) {
    var factorList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var variableList = factorList.concat(covariatesList);
    var modelTerms = context.cloneArray(ui.modelTerms.value(), []); 
    var termsList=[];
    termsList = context.getCombinations(variableList);
    termsList=unique(termsList.concat(modelTerms));
    context.sortArraysByLength(termsList);
//    ui.randomSupplier.setValue(context.valuesToItems(termsList, FormatDef.term));
    var clusterList = context.cloneArray(ui.cluster.value(), []);
    if (clusterList.length<1) {
                ui.randomSupplier.setValue(context.valuesToItems([], rtermFormat));                  return;
    }
    termsList.unshift(["Intercept"]);
    var alist=[];
    for (var i=0; i < clusterList.length; i++) {
     for (var j = 0; j < termsList.length; j++) {
       var item=context.cloneArray(termsList[j]);
       item[item.length]=clusterList[i];
       alist.push(item);
//       console.log(item);
     }
    }
    context.sortArraysByLength(alist);
    console.log("random supplierList");
    console.log(alist);
      var formatted=context.valuesToItems(alist, rtermFormat);
//    var busyList = context.cloneArray(ui.randomTerms.value(), []);
//    var busyForm = context.valuesToItems(busyList, rtermFormat);
//    var xunique = formatted.filter(function(val) {
//         return busyForm.indexOf(val) == -1;
//            });    
    ui.randomSupplier.setValue(formatted);
    
    console.log("updating random blocks");
    
    var randomTerms = context.cloneArray(ui.randomTerms.value(), []); 
    if (randomTerms.length==0)
        for(var i = 0; i < clusterList.length; ++i) {
           randomTerms[i]=[];
     }

    var missing = clusterList.length-randomTerms.length
    
    console.log("missing"+missing);

    console.log(randomTerms);
    
    for(var i = 0; i < missing; ++i) {
      randomTerms[randomTerms.length+1]=[];
    }
    console.log(randomTerms);
    ui.randomTerms.setValue(randomTerms);

};


var filterRandomTerms = function(ui, context) {
    console.log("filter random effects");  
    var termsList = context.cloneArray(ui.randomTerms.value(), []);
    console.log(termsList);
    var unique = termsList.filter((v, i, a) => a.indexOf(v) === i); 
    if (unique.length!=termsList.length)
      ui.randomTerms.setValue(unique);
  
};

var unique=function(arr) {
    var u = {}, a = [];
    for(var i = 0, l = arr.length; i < l; ++i){
        var prop=ssort(JSON.stringify(arr[i]));
        if(!u.hasOwnProperty(prop) && arr[i].length>0) {
            a.push(arr[i]);
            u[prop] = 1;
        }
    }
    return a;
};


var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
}

var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos]
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue
        }
        return cosmos;
}

var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};



// remove a list or a item from list
// order=0 remove only if term and target term are equal
// order>0 remove if term length>=order 
// for instance, order=1 remove any matching interaction with terms, keeps main effects
// order=2 remove from 3-way interaction on (keep up to 2-way interactions)

var removeFromList = function(quantum, cosmos, context, order = 1) {

     cosmos=normalize(cosmos);
     quantum=normalize(quantum);
     if (cosmos===undefined)
        return([]);
     var cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};


var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};



module.exports = events;

