var rtermFormat = require('./rtermFormat');

const events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
        updateSimpleSupplier(ui, this);
        updatePlotsSupplier(ui, this);


        if (typeof ui.comparison !== 'undefined' ) {
              fix_comparison(ui,this);
        }
        
        if (typeof ui.propodds_test !== 'undefined' ) {
          
            if (ui.model_type.getValue()==="ordinal") {
              ui.propodds_test.$el.show();
            } else {
              ui.propodds_test.$el.hide();
            }
        }


    },

    onChange_factors: function(ui) {
        calcModelTerms(ui, this);

    },

    onChange_covariates: function(ui) {
        calcModelTerms(ui, this);

    },

    onChange_model_terms: function(ui) {
        filterModelTerms(ui, this);
        updatePostHocSupplier(ui, this);
        updateSimpleSupplier(ui, this);
        updateEmmeansSupplier(ui, this);
        updatePlotsSupplier(ui, this);

    },
    onChange_nested_terms: function(ui) {
    },
  
    onChange_nested_add: function(ui) {

      let tvalues=this.cloneArray(ui.model_terms.value(),[]);
      let nvalues=this.cloneArray(ui.nested_terms.value(),[]);
      var filtered = nvalues.filter(function(item) {
         return tvalues.some(function(jtem) {
           return FormatDef.term.isEqual(jtem, item);
         });
      });
      if (filtered.length !== nvalues.length)
                  ui.nested_terms.setValue(filtered);
    },
    onEvent_comparison: function(ui) {
         
         fix_comparison(ui, this);

    },
    onChange_plotsSupplier: function(ui) {
        let values = this.itemsToValues(ui.plotsSupplier.value());
        this.checkValue(ui.plotHAxis, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepLines, false, values, FormatDef.variable);
        this.checkValue(ui.plotSepPlots, true, values, FormatDef.variable);
    },
    
    onChange_simpleSupplier: function(ui) {
        let values = this.itemsToValues(ui.simpleSupplier.value());
        this.checkValue(ui.simple_effects, false, values, FormatDef.variable);
        this.checkValue(ui.simple_moderators, true, values, FormatDef.variable);
    },

    onUpdate_simpleSupplier: function(ui) {
        updateSimpleSupplier(ui, this);
    },
    onUpdate_plotsSupplier: function(ui) {
        updatePlotsSupplier(ui, this);
    },

     onChange_model: function(ui) {

        if (typeof ui.es_RR !== 'undefined' ) {
              ui.es_RR.setValue(false);
        }
        if (ui.model_type.getValue()==="custom" ||  ui.model_type.getValue()==="linear")        {
               ui.es_expb.setValue(false);
               ui.estimates_ci.setValue(true);
               ui.expb_ci.setValue(false);
        } else  {
               ui.es_expb.setValue(true);
               ui.estimates_ci.setValue(false);
               ui.expb_ci.setValue(true);
        }

        if (typeof ui.propodds_test !== 'undefined') {
              if (ui.model_type.getValue()==="ordinal") {
                  ui.propodds_test.$el.show();
              } else {
                  ui.propodds_test.$el.hide();
              }
        }

        if (  ui.model_type.getValue() === 'ordinal') {
              ui.ci_method_wald.setValue(true);
              ui.ci_method_quantile.setEnabled(false);
              ui.ci_method_ci_method_bcai.setEnabled(false);
        }
  
        ui.dep.setValue(null);
      },

    onChange_model_remove: function(ui) {
      let values=this.cloneArray(ui.model_terms.value(),[]);
      this.checkValue(ui.nested_terms, true, values, FormatDef.term);
      
    },
    onChange_posthocSupplier: function(ui) {
        let values = this.itemsToValues(ui.posthocSupplier.value());
        this.checkValue(ui.posthoc, true, values, FormatDef.term);
    },

    onUpdate_posthocSupplier: function(ui) {
        updatePostHocSupplier(ui, this);
    },

    onChange_emmeansSupplier: function(ui) {
        let values = this.itemsToValues(ui.emmeansSupplier.value());
        this.checkValue(ui.emmeans, true, values, FormatDef.term);

    },

    onUpdate_emmeansSupplier: function(ui) {
        updateEmmeansSupplier(ui, this);
    },

    
    onUpdate_modelSupplier: function(ui) {
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.modelSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));
    }

};

var fix_comparison=function(ui, context) {
  
            if (ui.comparison.getValue()===true) {
              
              ui.nested_layout.$buttons.show();
              ui.nested_layout.$label.show();
              ui.nested_layout.container.$el.show();
              ui.model_terms.$el.height("113px");

            } else {
              ui.nested_layout.$buttons.hide();
              ui.nested_layout.$label.hide();
              ui.nested_layout.container.$el.hide();
              ui.nested_terms.setValue([]);
              ui.model_terms.$el.height("246.315px");
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
     
    updateContrasts(ui, variableList, context);
    updateScaling(ui, covariatesList, context);
};

var updateSimpleSupplier = function(ui, context) {
      
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
    };

var updatePlotsSupplier = function(ui, context) {

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
    
    };


var updatePostHocSupplier = function(ui, context) {
    var termsList = context.cloneArray(ui.model_terms.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];
        if (containsCovariate(term, covariatesList) === false)
            list.push(term);
    }
    ui.posthocSupplier.setValue(context.valuesToItems(list, FormatDef.term));
};

var updateEmmeansSupplier = function(ui, context) {
    var termsList = context.cloneArray(ui.model_terms.value(), []);
    var list = [];
    for (var j = 0; j < termsList.length; j++) {
        var term = termsList[j];

        if (unique(term).length===term.length)
              list.push(term);
    }
    ui.emmeansSupplier.setValue(context.valuesToItems(list, FormatDef.term));

};




var filterModelTerms = function(ui, context) {
  
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
};



var containsCovariate = function(value, covariates) {
    for (var i = 0; i < covariates.length; i++) {
        if (FormatDef.term.contains(value, covariates[i]))
            return true;
    }

    return false;
};

var unique = function(avec) {
  
  return(avec.filter((v, i, a) => a.indexOf(v) === i));
};

var mark = function(obj) {
  
   console.log(obj);
};


module.exports = events;

