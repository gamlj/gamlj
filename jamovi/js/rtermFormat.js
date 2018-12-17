var rtermFormat = new Format ({
  
  name: 'term',
  
  default: null,
  
  toString: function(raw) {
    return rtermFormat._itemToString(raw, 0);
  },
  
  parse: function(value) {
    return "test";
  },
  
  isValid: function(raw) {
    return rtermFormat._validateItem(raw, 0);
  },
  
  isEqual: function(raw1, raw2) {
    return rtermFormat._areItemsEqual(raw1, raw2);
  },
  
  isEmpty: function(raw) {
    return raw === null;
  },
  
  contains: function(raw, value) {
    
    var type1 = typeof raw;
    var type2 = typeof value;
    
    if (type1 === 'string' && type2 === 'string')
      return raw === value;
      else if (type1 === 'string')
        return false;
      
      for (var j = 0; j < raw.length; j++) {
        
        if (rtermFormat.contains(raw[j], value))
          return true;
      }
      
      if (raw.length < value.length)
        return false;
      
      var jStart = 0;
      for (var i = 0; i < value.length; i++) {
        var found = false;
        for (var k = jStart; k < raw.length; k++) {
          if (rtermFormat._areItemsEqual(value[i], raw[k])) {
            if (jStart === k)
              jStart = k + 1;
            found = true;
            break;
          }
        }
        
        if (found === false)
          return false;
      }
      
      return true;
  },
  
  _areItemsEqual: function(item1, item2) {
    var type1 = typeof item1;
    var type2 = typeof item1;
    
    if (type1 !== type2)
      return false;
    
    if (type1=== 'string' && type2 === 'string')
      return item1 === item2;
      
      if (Array.isArray(item1) === false || Array.isArray(item2) === false)
        return false;
      
      if (item1.length !== item2.length)
        return false;
      
      var jStart = 0;
      for (var i = 0; i < item1.length; i++) {
        var found = false;
        for (var j = jStart; j < item2.length; j++) {
          if (rtermFormat._areItemsEqual(item1[i], item2[j])) {
            if (j === jStart)
              jStart = j + 1;
            found = true;
            break;
          }
        }
        if (found === false)
          return false;
      }
      
      return true;
  },
  
  _getJoiner: function(level) {
    if (level === 0)
    return ':';
//      return '✻';
//        return 'X';
    
    return '|';
  },

  getSuperscript: function(value) {
        return '<sup> ' + value + '</sup>';
    },
  
  _itemToString: function(item, level, power) {
//    console.log("rterm format used")
    if (typeof item === 'string')
         return item + (power > 1 ? this.getSuperscript(power) : '');

    var joiner = rtermFormat._getJoiner(level);

        let combined = '';
        let npower = 1;
        for (let i = 0; i < item.length; i++) {
            if (i < item.length - 1 && item[i] === item[i+1])
                npower += 1;
            else {
              if (i===(item.length-1))
                    joiner='|';
               combined = (combined !== '' ? (combined + ' ' + joiner + ' ') : '') + FormatDef.term._itemToString(item[i], level + 1, npower);
                npower = 1;
            }
}
    return combined;
  },
  
  _validateItem: function(item, level) {
    if (level > 0 && typeof item === 'string')
      return true;
    else if (level > 2 || Array.isArray(item) === false || item.length === 0)
      return false;
    
    for (var i = 0; i < item.length; i++) {
      if (rtermFormat._validateItem(item[i], level + 1) === false)
        return false;
    }
    
    return true;
  }
});

module.exports = rtermFormat;