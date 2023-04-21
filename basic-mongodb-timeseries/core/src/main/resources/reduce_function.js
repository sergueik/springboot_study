function(key, values) {

	var res = { total: 0, count: 0, min: 0, max: 0, mean:0, ts: null };
    values.forEach(function(v) {
    	if ( v.total > 0 ){
		    res.total += v.total;
	    }
    	//number of events
        res.count += v.count;
		if ( v.min < res.min ) {
           res.min = v.min;     
        }   
		if ( v.max > res.max ) {
           res.max = v.max;     
        }   
    });
    return res;
}