var rtermFormat = require('./rtermFormat');

const fun = {

    calcModelTerms: function(ui, context) {
    var variableList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var combinedList = variableList.concat(covariatesList);
    ui.modelSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.plotsSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
    ui.simpleSupplier.setValue(context.valuesToItems(combinedList, FormatDef.variable));
 
    var diff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    var diff2 = context.findChanges("covariatesList", covariatesList, true, FormatDef.variable);
    var combinedDiff = context.findChanges("combinedList", combinedList, true, FormatDef.variable);


    var termsList = context.cloneArray(ui.model_terms.value(), []);
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

    if (termsChanged) {
        ui.model_terms.setValue(termsList);
     }
     
    this.updateContrasts(ui, variableList, context);
    this.updateScaling(ui, covariatesList, context);
} ,
  
  updateContrasts: function(ui, variableList, context) {
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
},
  updateScaling: function(ui, variableList, context) {
    var currentList = context.cloneArray(ui.covs_scale.value(), []);

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
    ui.covs_scale.setValue(list3);
},

 updatePostHocSupplier: function(ui, context) {
    var termsList = context.cloneArray(ui.model_terms.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];
        if (containsCovariate(term, covariatesList) === false)
            list.push(term);
    }
    ui.posthocSupplier.setValue(context.valuesToItems(list, FormatDef.term));
},

  updateEmmeansSupplier: function(ui, context) {
    var termsList = context.cloneArray(ui.model_terms.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];

        if (unique(term).length===term.length)
              list.push(term);
    }
    ui.emmeansSupplier.setValue(context.valuesToItems(list, FormatDef.term));

},

   updatePlotsSupplier: function(ui, context) {

        var termsList = context.cloneArray(ui.model_terms.value(), []);
        var varList=[];
        for (var j = 0; j < termsList.length; j++) {
            var newTerm=context.clone(termsList[j]);
            if (newTerm.length==1) {
                  varList.push(newTerm[0]); // was varList.push(newTerm);
            }
        }
        varList=context.valuesToItems(varList, FormatDef.variable);
        ui.plotsSupplier.setValue(varList);
    
    },

 filterModelTerms: function(ui, context) {
  
    var termsList = context.cloneArray(ui.model_terms.value(), []);
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
        ui.model_terms.setValue(termsList);
},
  updateSimpleSupplier: function(ui, context) {
      
        var termsList = context.cloneArray(ui.model_terms.value(), []);
        var varList=[];
        for (var j = 0; j < termsList.length; j++) {
            var newTerm=context.clone(termsList[j]);
            if (newTerm.length==1) {
                  varList.push(newTerm[0]); // was varList.push(newTerm);
            }
        }
        varList=context.valuesToItems(varList, FormatDef.variable);
        ui.simpleSupplier.setValue(varList);
    },

   fix_comparison:function(ui, context) {
  
            if (ui.comparison.getValue()===true) {
              
              ui.nested_layout.$buttons.show();
              ui.nested_layout.$label.show();
              ui.nested_layout.container.$el.show();
              ui.model_terms.$el.height("113px");
             
             if (ui.nested_re !== undefined) { 
                 ui.nested_re_layout.$buttons.show();
                 ui.nested_re_layout.$label.show();
                 ui.nested_re_layout.container.$el.show();
                 ui.re.$el.height("113px");
                 var renested=context.cloneArray(ui.nested_re.value(), [[]]);
                 if (renested[0].length === 0) {
                         var relist=context.cloneArray(ui.re.value(), [[]]);
                         ui.nested_re.setValue(relist);
                 }
             }

            } else {
              ui.nested_layout.$buttons.hide();
              ui.nested_layout.$label.hide();
              ui.nested_layout.container.$el.hide();
              ui.nested_terms.setValue([]);
              ui.model_terms.$el.height("246.315px");

              if (ui.nested_re !== undefined) { 
             
                  ui.nested_re.setValue([[]]);
                  ui.nested_re_layout.$buttons.hide();
                  ui.nested_re_layout.$label.hide();
                  ui.nested_re_layout.container.$el.hide();
                  ui.re.$el.height("243.315px");
                  
              }

              
            }


         },
         
  fixRandomEffects: function(ui, context) {
         
            var option=ui.re_corr.value();
            var oldOption = context.workspace.re_corr;
            context.workspace.re_corr=option;
            
            if (ui.re_corr.value()=="block") {
                  if (oldOption==="corr" || oldOption==="nocorr")
                        ui.re.setValue(Array([]));
                  // make sure the add button is visible                      
                  var button= ui.re.$addButton;
                  button[0].style.visibility="visible";
                  // get the re field to manipulate the children
                  var target= ui.re;
                  target.$el[0].lastElementChild.style.borderColor=null;
                  target.controls[0].$el[0].childNodes[0].style.visibility="visible";
                  // remove possibility to kill the first row
                  target.controls[0].$el[0].childNodes[0].style.visibility="hidden";
                  

             } else {
                 var data = context.cloneArray(ui.re.value(),[]);
                 var one = flatMulti(data,context);
                 var button= ui.re.$addButton;
                 button[0].style.visibility="hidden";
                 var target= ui.re;
                 target.setValue(Array(one));
                 var one = target.controls[0];
                 target.$el[0].lastElementChild.style.borderColor="transparent";
                 one.$el[0].childNodes[0].style.visibility="hidden";
                 one.$el[0].childNodes[1].childNodes[0].style.borderStyle="unset";
             }
             
             // handle the nested random effects
                 // be sure there's at least one slot available
                 if (ui.nested_re.value().length===0)
                         ui.nested_re.setValue([[]]);
                  // remove possibility to kill the first row
                  ui.nested_re.controls[0].$el[0].childNodes[0].style.visibility="hidden";


  
},

 updateRandomSupplier: function(ui, context) {
   
   if (typeof ui.randomSupplier == 'undefined' ) {
              return;
        }

    console.log("updating random supplier");
// first we check if the update is needed    
    var clusterList = context.cloneArray(ui.cluster.value(), []);
    if (clusterList.length<1) {
                ui.randomSupplier.setValue(context.valuesToItems([], rtermFormat));
                return;
    }
    var termsList=[];
// then we check how to prepare the list

    var order = 0

    if ( ui.re_modelterms.value() === true) {
         termsList = context.cloneArray(ui.model_terms.value(), []); 
    }
    var option = ui.re_listing.value();

    if (  option != "none" ) {
       var factorList = context.cloneArray(ui.factors.value(), []);
       var covariatesList = context.cloneArray(ui.covs.value(), []);
       var variablesList = factorList.concat(covariatesList);

        if ( option === "main") order = 1;
        if ( option === "way2") order = 2;
        if ( option === "way3") order = 3;
        if ( option === "all")  order = variablesList.length
        
        termsList = unique(termsList.concat(interactions(variablesList, order)));
        
    }
    termsList.unshift(["Intercept"]);

    var alist=[];
    for (var i=0; i < clusterList.length; i++) {
     for (var j = 0; j < termsList.length; j++) {
       var item=context.cloneArray(termsList[j]);
       item[item.length]=clusterList[i];
       alist.push(item);
     }
    }
    var formatted=context.valuesToItems(alist, rtermFormat);

    ui.randomSupplier.setValue(formatted);
  

},


         mark: function(obj) {
               console.log(obj);
         }

}



module.exports=fun

var mark = function(obj) {
  console.log(obj);
};

var unique = function(avec) {
  return(avec.filter((v, i, a) => a.indexOf(v) === i));
};


var containsCovariate = function(value, covariates) {
  for (var i = 0; i < covariates.length; i++) {
    if (FormatDef.term.contains(value, covariates[i]))
      return true;
  }
  
  return false;
};

function interactions(set,order) {

    var inter=[], j
    for (j = 0; j < order; j++) {
        inter=inter.concat(k_combinations(set,j+1))
    }
    return inter;
};

function k_combinations(set, k) {
	var i, j, combs, head, tailcombs;
	
	if (k > set.length || k <= 0) {
		return [];
	}
	
	// K-sized set has only one K-sized subset.
	if (k == set.length) {
		return [set];
	}
	
	// There is N 1-sized subsets in a N-sized set.
	if (k == 1) {
		combs = [];
		for (i = 0; i < set.length; i++) {
			combs.push([set[i]]);
		}
		return combs;
	}
	
	combs = [];
	for (i = 0; i < set.length - k + 1; i++) {
		head = set.slice(i, i + 1);
		tailcombs = k_combinations(set.slice(i + 1), k - 1);
		for (j = 0; j < tailcombs.length; j++) {
			combs.push(head.concat(tailcombs[j]));
		}
	}
	return combs;
};

var flatMulti = function(cosmos,context) {
  var light = []
  for (var i=0 ; i < cosmos.length; i++) {
    light=addToList(light,cosmos[i],context);
  }
  return unique(light);
};

var addToList = function(quantum, cosmos, context) {
  
    cosmos = normalize(context.cloneArray(cosmos));
    quantum = normalize(context.cloneArray(quantum));
    
    for (var i = 0; i < quantum.length; i++) {
          if (dim(quantum[i])===0)
              cosmos.push([quantum[i]]);
          if (dim(quantum[i])===1)
              cosmos.push(quantum[i]);
          }
    return unique(cosmos);
};

var flatMulti = function(cosmos,context) {
  var light = []
  for (var i=0 ; i < cosmos.length; i++) {
    light=addToList(light,cosmos[i],context);
  }
  return unique(light);
};


var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
};

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
