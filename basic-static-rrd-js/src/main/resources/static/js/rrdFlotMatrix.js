/*
 * RRD graphing libraries, based on Flot
 * Part of the javascriptRRD package
 * Copyright (c) 2010 Frank Wuerthwein, fkw@ucsd.edu
 *                    Igor Sfiligoi, isfiligoi@ucsd.edu
 *
 * Original repository: http://javascriptrrd.sourceforge.net/
 * 
 * MIT License [http://www.opensource.org/licenses/mit-license.php]
 *
 */

/*
 *
 * Flot is a javascript plotting library developed and maintained by
 * Ole Laursen [http://www.flotcharts.org/]
 *
 */

/*
 * The rrd_files is a list of 
 *  [rrd_id,rrd_file] pairs
 * All rrd_files must have the same step, the same DSes and the same number of RRAs.
 *
 */ 

/*
 * The ds_list is a list of 
 *  [ds_id, ds_title] pairs
 * If not defined, the list will be created from the RRDs
 *
 */ 

/*
 * Local dependencies:
 *  rrdFlotSupport.py
 *
 * External dependencies:
 *  [Flot]/jquery.py
 *  [Flot]/jquery.flot.js
 *  [Flot]/jquery.flot.selection.js
 */

/* graph_options defaults (see Flot docs for details)
 * {
 *  legend: { position:"nw",noColumns:3},
 *  lines: { show:true },
 *  yaxis: { autoscaleMargin: 0.20}
 * }
 *
 * rrd_graph_options is a dictionary of rrd_id, 
 *   with each element being a graph_option
 *   The defaults for each element are
 *   {
 *     title: label  or rrd_name                          // this is what is displayed in the checkboxes
 *     checked: true                                      // boolean
 *     label: title or rrd_name                           // this is what is displayed in the legend
 *     color: rrd_index                                   // see Flot docs for details
 *     lines: { show:true, fill: true, fillColor:color }  // see Flot docs for details
 *   }
 *
 * //overwrites other defaults; mostly used for linking via the URL
 * rrdflot_defaults defaults (see Flot docs for details) 	 
 * {
 *    graph_only: false        // If true, limit the display to the graph only
 *    legend: "Top"            //Starting location of legend. Options are: 
 *                             //   "Top","Bottom","TopRight","BottomRight","None".
 *    num_cb_rows: 12          //How many rows of DS checkboxes per column.
 *    use_element_buttons: false  //To be used in conjunction with num_cb_rows: This option
 *                             //    creates a button above every column, which selects
 *                             //    every element in the column. 
 *    multi_rra: false         //"true" appends the name of the RRA consolidation function (CF) 
 *                             //    (AVERAGE, MIN, MAX or LAST) to the name of the RRA. Useful 
 *                             //    for RRAs over the same interval with different CFs.  
 *    use_checked_RRDs: false   //Use the list checked_RRDs below.
 *    checked_RRDs: []          //List of elements to be checked by default when graph is loaded. 
 *                             //    Overwrites graph options. 
 *    use_rra: false           //Whether to use the rra index specified below.
 *    rra: 0                   //RRA (rra index in rrd) to be selected when graph is loaded. 
 *    use_windows: false       //Whether to use the window zoom specifications below.
 *    window_min: 0            //Sets minimum for window zoom. X-axis usually in unix time. 
 *    window_max: 0            //Sets maximum for window zoom.
 *    graph_height: "300px"    //Height of main graph. 
 *    graph_width: "500px"     //Width of main graph.
 *    scale_height: "110px"    //Height of small scaler graph.
 *    scale_width: "250px"     //Width of small scaler graph.
 * } 
 */

var local_checked_RRDs = [];
var selected_rra = 0;
var window_min=0;
var window_max=0;
var elem_group=null;


function rrdFlotMatrix(html_id, rrd_files, ds_list, graph_options, rrd_graph_options,rrdflot_defaults) {
  this.html_id=html_id;
  this.rrd_files=rrd_files;
  if (rrdflot_defaults==null) {
    this.rrdflot_defaults=new Object(); // empty object, just not to be null
  } else {
    this.rrdflot_defaults=rrdflot_defaults;
  }
  if (ds_list==null) {
    this.ds_list=[];
    var rrd_file=this.rrd_files[0][1]; // get the first one... they are all the same
    var nrDSs=rrd_file.getNrDSs();
    for (var i=0; i<nrDSs; i++) {
      var ds=this.rrd_files[0][1].getDS(i);
      var name=ds.getName();
      this.ds_list.push([name,name]);
    }
  } else {
    this.ds_list=ds_list;
  }
  this.graph_options=graph_options;
  if (rrd_graph_options==null) {
    this.rrd_graph_options=new Object(); // empty object, just not to be null
  } else {
    this.rrd_graph_options=rrd_graph_options;
  }
  this.selection_range=new rrdFlotSelection();

  this.createHTML();
  this.populateDS();
  this.populateRes();
  this.populateRRDcb();
  this.drawFlotGraph()

  if (this.rrdflot_defaults.graph_only==true) {
    this.cleanHTMLCruft();
  }
}


// ===============================================
// Create the HTML tags needed to host the graphs
rrdFlotMatrix.prototype.createHTML = function() {
  var rf_this=this; // use obj inside other functions

  var base_el=document.getElementById(this.html_id);

  this.ds_id=this.html_id+"_ds";
  this.res_id=this.html_id+"_res";
  this.rrd_cb_id=this.html_id+"_rrd_cb";
  this.graph_id=this.html_id+"_graph";
  this.scale_id=this.html_id+"_scale";
  this.legend_sel_id=this.html_id+"_legend_sel";

  // First clean up anything in the element
  while (base_el.lastChild!=null) base_el.removeChild(base_el.lastChild);

  // Now create the layout
  var external_table=document.createElement("Table");
  this.external_table=external_table;

  // DS rows: select DS
  var rowDS=external_table.insertRow(-1);
  var cellDS=rowDS.insertCell(-1);
  cellDS.colSpan=4
  cellDS.appendChild(document.createTextNode("Element:"));
  var forDS=document.createElement("Select");
  forDS.id=this.ds_id;
  forDS.onchange= function () {rf_this.callback_ds_changed();};
  cellDS.appendChild(forDS);

  // Header row: resulution select and DS selection title
  var rowHeader=external_table.insertRow(-1);
  var cellRes=rowHeader.insertCell(-1);
  cellRes.colSpan=3;
  cellRes.appendChild(document.createTextNode("Resolution:"));
  var forRes=document.createElement("Select");
  forRes.id=this.res_id;
  forRes.onchange= function () {rf_this.callback_res_changed();};
  cellRes.appendChild(forRes);

  var cellRRDTitle=rowHeader.insertCell(-1);
  cellRRDTitle.appendChild(document.createTextNode("Select RRDs to plot:"));

  // Graph row: main graph and DS selection block
  var rowGraph=external_table.insertRow(-1);
  var cellGraph=rowGraph.insertCell(-1);
  cellGraph.colSpan=3;
  var elGraph=document.createElement("Div");
  if(this.rrdflot_defaults.graph_width!=null) {
     elGraph.style.width=this.rrdflot_defaults.graph_width;
  } else {elGraph.style.width="500px";}
  if(this.rrdflot_defaults.graph_height!=null) {
     elGraph.style.height=this.rrdflot_defaults.graph_height;
  } else {elGraph.style.height="300px";}
  elGraph.id=this.graph_id;
  cellGraph.appendChild(elGraph);

  var cellRRDcb=rowGraph.insertCell(-1);
  cellRRDcb.vAlign="top";
  var formRRDcb=document.createElement("Form");
  formRRDcb.id=this.rrd_cb_id;
  formRRDcb.onchange= function () {rf_this.callback_rrd_cb_changed();};
  cellRRDcb.appendChild(formRRDcb);

  // Scale row: scaled down selection graph
  var rowScale=external_table.insertRow(-1);

  var cellScaleLegend=rowScale.insertCell(-1);
  cellScaleLegend.vAlign="top";
  cellScaleLegend.appendChild(document.createTextNode("Legend:"));
  cellScaleLegend.appendChild(document.createElement('br'));
  var forScaleLegend=document.createElement("Select");
  forScaleLegend.id=this.legend_sel_id;
  forScaleLegend.appendChild(new Option("Top","nw",this.rrdflot_defaults.legend=="Top",this.rrdflot_defaults.legend=="Top"));
  forScaleLegend.appendChild(new Option("Bottom","sw",this.rrdflot_defaults.legend=="Bottom",this.rrdflot_defaults.legend=="Bottom"));
  forScaleLegend.appendChild(new Option("TopRight","ne",this.rrdflot_defaults.legend=="TopRight",this.rrdflot_defaults.legend=="TopRight"));
  forScaleLegend.appendChild(new Option("BottomRight","se",this.rrdflot_defaults.legend=="BottomRight",this.rrdflot_defaults.legend=="BottomRight"));
  forScaleLegend.appendChild(new Option("None","None",this.rrdflot_defaults.legend=="None",this.rrdflot_defaults.legend=="None"));
  forScaleLegend.onchange= function () {rf_this.callback_legend_changed();};
  cellScaleLegend.appendChild(forScaleLegend);

  var cellScale=rowScale.insertCell(-1);
  cellScale.align="right";
  var elScale=document.createElement("Div");
  if(this.rrdflot_defaults.scale_width!=null) {
     elScale.style.width=this.rrdflot_defaults.scale_width;
  } else {elScale.style.width="250px";}
  if(this.rrdflot_defaults.scale_height!=null) {
     elScale.style.height=this.rrdflot_defaults.scale_height;
  } else {elScale.style.height="110px";}
  elScale.id=this.scale_id;
  cellScale.appendChild(elScale);
  
  var cellScaleReset=rowScale.insertCell(-1);
  cellScaleReset.vAlign="top";
  cellScaleReset.appendChild(document.createTextNode(" "));
  cellScaleReset.appendChild(document.createElement('br'));
  var elScaleReset=document.createElement("input");
  elScaleReset.type = "button";
  elScaleReset.value = "Reset selection";
  elScaleReset.onclick = function () {rf_this.callback_scale_reset();}
  cellScaleReset.appendChild(elScaleReset);


  base_el.appendChild(external_table);
};

// ===============================================
// Remove all HTMl elements but the graph
rrdFlotMatrix.prototype.cleanHTMLCruft = function() {
  var rf_this=this; // use obj inside other functions

  // delete 2 top and 1 bottom rows... graph is in the middle
  this.external_table.deleteRow(-1);
  this.external_table.deleteRow(0);
  this.external_table.deleteRow(0);

  var rrd_el=document.getElementById(this.rrd_cb_id);
  rrd_el.removeChild(rrd_el.lastChild);
}

// ======================================
// Populate DSs, RRA and RRD info
rrdFlotMatrix.prototype.populateDS = function() {
  var form_el=document.getElementById(this.ds_id);

  // First clean up anything in the element
  while (form_el.lastChild!=null) form_el.removeChild(form_el.lastChild);

  for (i in this.ds_list) {
    var ds=this.ds_list[i];
    form_el.appendChild(new Option(ds[1],ds[0]));
  }
};

rrdFlotMatrix.prototype.populateRes = function() {
  var form_el=document.getElementById(this.res_id);

  // First clean up anything in the element
  while (form_el.lastChild!=null) form_el.removeChild(form_el.lastChild);

  var rrd_file=this.rrd_files[0][1]; // get the first one... they are all the same
  // now populate with RRA info
  var nrRRAs=rrd_file.getNrRRAs();
  for (var i=0; i<nrRRAs; i++) {
    var rra=rrd_file.getRRAInfo(i);
    var step=rra.getStep();
    var rows=rra.getNrRows();
    var period=step*rows;
    var rra_label=rfs_format_time(step)+" ("+rfs_format_time(period)+" total)";
    if (this.rrdflot_defaults.multi_rra) rra_label+=" "+rra.getCFName();
    form_el.appendChild(new Option(rra_label,i));
  }
  if(this.rrdflot_defaults.use_rra) {form_el.selectedIndex = this.rrdflot_defaults.rra;}
};

rrdFlotMatrix.prototype.populateRRDcb = function() {
  var rf_this=this; // use obj inside other functions
  var form_el=document.getElementById(this.rrd_cb_id);
 
  //Create a table within a table to arrange
  // checkbuttons into two or more columns
  var table_el=document.createElement("Table");
  var row_el=table_el.insertRow(-1);
  row_el.vAlign="top";
  var cell_el=null; // will define later

  if (this.rrdflot_defaults.num_cb_rows==null) {
     this.rrdflot_defaults.num_cb_rows=12; 
  }
  // now populate with RRD info
  var nrRRDs=this.rrd_files.length;
  var elem_group_number = 0;
 
  for (var i=0; i<nrRRDs; i++) {

    if ((i%this.rrdflot_defaults.num_cb_rows)==0) { // one column every x RRDs
      if(this.rrdflot_defaults.use_element_buttons) {
        cell_el=row_el.insertCell(-1); //make next element column 
        if(nrRRDs>this.rrdflot_defaults.num_cb_rows) { //if only one column, no need for a button
          elem_group_number = (i/this.rrdflot_defaults.num_cb_rows)+1;
          var elGroupSelect = document.createElement("input");
          elGroupSelect.type = "button";
          elGroupSelect.value = "Group "+elem_group_number;
          elGroupSelect.onclick = (function(e) { //lambda function!!
             return function() {rf_this.callback_elem_group_changed(e);};})(elem_group_number);

          cell_el.appendChild(elGroupSelect);
          cell_el.appendChild(document.createElement('br')); //add space between the two
        }
      } else {
         //just make next element column
         cell_el=row_el.insertCell(-1); 
      }
    }

    var rrd_el=this.rrd_files[i];
    var rrd_file=rrd_el[1];
    var name=rrd_el[0];
    var title=name;
 
    if(this.rrdflot_defaults.use_checked_RRDs) {
       if(this.rrdflot_defaults.checked_RRDs.length==0) {
          var checked=(i==0); // only first checked by default
       } else{checked=false;}
    } else {var checked=(i==0);}
    if (this.rrd_graph_options[name]!=null) {
      var rgo=this.rrd_graph_options[name];
      if (rgo['title']!=null) {
	// if the user provided the title, use it
	title=rgo['title'];
      } else if (rgo['label']!=null) {
	// use label as a second choiceit
	title=rgo['label'];
      } // else leave the rrd name
      if(this.rrdflot_defaults.use_checked_RRDs) {
         if(this.rrdflot_defaults.checked_RRDs.length==0) {
           // if the user provided the title, use it
           checked=rgo['checked'];
         }
      } else {
         if (rgo['checked']!=null) {
            checked=rgo['checked']; 
         }
      }
    }
    if(this.rrdflot_defaults.use_checked_RRDs) {
       if(this.rrdflot_defaults.checked_RRDs==null) {continue;}
       for(var j=0;j<this.rrdflot_defaults.checked_RRDs.length;j++){
             if (name==this.rrdflot_defaults.checked_RRDs[j]) {checked=true;}
       }
    }
    var cb_el = document.createElement("input");
    cb_el.type = "checkbox";
    cb_el.name = "rrd";
    cb_el.value = i;
    cb_el.checked = cb_el.defaultChecked = checked;
    cell_el.appendChild(cb_el);
    cell_el.appendChild(document.createTextNode(title));
    cell_el.appendChild(document.createElement('br'));
  }
  form_el.appendChild(table_el);
};

// ======================================
// 
rrdFlotMatrix.prototype.drawFlotGraph = function() {
  // DS
  var oSelect=document.getElementById(this.ds_id);
  var ds_id=oSelect.options[oSelect.selectedIndex].value;

  // Res contains the RRA idx
  oSelect=document.getElementById(this.res_id);
  var rra_idx=Number(oSelect.options[oSelect.selectedIndex].value);

  selected_rra=rra_idx;
  if(this.rrdflot_defaults.use_rra) {
    oSelect.options[oSelect.selectedIndex].value = this.rrdflot_defaults.rra;
    rra_idx = this.rrdflot_defaults.rra;
  }

  // Extract ds info ... to be finished
  var ds_positive_stack=null;

  var std_colors=["#00ff00","#00ffff","#0000ff","#ff00ff",
		  "#808080","#ff0000","#ffff00","#e66266",
		  "#33cccc","#fff8a9","#ccffff","#a57e81",
		  "#7bea81","#8d4dff","#ffcc99","#000000"];

  // now get the list of selected RRDs
  var rrd_list=[];
  var rrd_colors=[];
  var oCB=document.getElementById(this.rrd_cb_id);
  var nrRRDs=oCB.rrd.length;
  if (oCB.rrd.length>0) {
    for (var i=0; i<oCB.rrd.length; i++) {
      if (oCB.rrd[i].checked==true) {
	//var rrd_idx=Number(oCB.rrd[i].value);
	rrd_list.push(this.rrd_files[i]);
	color=std_colors[i%std_colors.length];
	if ((i/std_colors.length)>=1) {
	  // wraparound, change them a little
	  idiv=Math.floor(i/std_colors.length);
	  c1=parseInt(color[1]+color[2],16);
	  c2=parseInt(color[3]+color[4],16);
	  c3=parseInt(color[5]+color[6],16);
	  m1=Math.floor((c1-128)/Math.sqrt(idiv+1))+128;
	  m2=Math.floor((c2-128)/Math.sqrt(idiv+1))+128;
	  m3=Math.floor((c3-128)/Math.sqrt(idiv+1))+128;
	  if (m1>15) s1=(m1).toString(16); else s1="0"+(m1).toString(16);
	  if (m2>15) s2=(m2).toString(16); else s2="0"+(m2).toString(16);
	  if (m3>15) s3=(m3).toString(16); else s3="0"+(m3).toString(16);
	  color="#"+s1+s2+s3;
	}
        rrd_colors.push(color);
      }
    }
  } else { // single element is not treated as an array
    if (oCB.rrd.checked==true) {
      // no sense trying to stack a single element
      rrd_list.push(this.rrd_files[0]);
      rrd_colors.push(std_colors[0]);
    }
  }
  
  // then extract RRA data about those DSs... to be finished
  var flot_obj=rrdRRAMultiStackFlotObj(rrd_list,rra_idx,ds_id);

  // fix the colors, based on the position in the RRD
  for (var i=0; i<flot_obj.data.length; i++) {
    var name=flot_obj.data[i].label; // at this point, label is the rrd_name
    var color=rrd_colors[flot_obj.data.length-i-1]; // stack inverts colors
    var lines=null;
    if (this.rrd_graph_options[name]!=null) {
      var dgo=this.rrd_graph_options[name];
      if (dgo['color']!=null) {
	color=dgo['color'];
      }
      if (dgo['label']!=null) {
	// if the user provided the label, use it
	flot_obj.data[i].label=dgo['label'];
      } else  if (dgo['title']!=null) {
	// use title as a second choice 
	flot_obj.data[i].label=dgo['title'];
      } // else use the rrd name
      if (dgo['lines']!=null) {
	// if the user provided the label, use it
	flot_obj.data[i].lines=dgo['lines'];
      }
    }
    if (lines==null) {
	flot_obj.data[i].lines= { show:true, fill: true, fillColor:color };
    }
    flot_obj.data[i].color=color;
  }

  // finally do the real plotting
  this.bindFlotGraph(flot_obj);
};

// ======================================
// Bind the graphs to the HTML tags
rrdFlotMatrix.prototype.bindFlotGraph = function(flot_obj) {
  var rf_this=this; // use obj inside other functions

  // Legend
  var oSelect=document.getElementById(this.legend_sel_id);
  var legend_id=oSelect.options[oSelect.selectedIndex].value;
  var graph_jq_id="#"+this.graph_id;
  var scale_jq_id="#"+this.scale_id;

  var graph_options = {
    legend: {show:false, position:"nw",noColumns:3},
    lines: {show:true},
    xaxis: { mode: "time" },
    yaxis: { autoscaleMargin: 0.20},
    selection: { mode: "x" },
    tooltip: true,
    tooltipOpts: { content: "<h4>%s</h4> Value: %y.3" },
    grid: { hoverable: true },
  };
  
  if (legend_id=="None") {
    // do nothing
  } else {
    graph_options.legend.show=true;
    graph_options.legend.position=legend_id;
  }

  if (this.graph_options!=null) {
    graph_options=populateGraphOptions(graph_options,this.graph_options);
  }

  if (graph_options.tooltip==false) {
    // avoid the need for the caller specify both
    graph_options.grid.hoverable=false;
  }


  if (this.selection_range.isSet()) {
    var selection_range=this.selection_range.getFlotRanges();
    if(this.rrdflot_defaults.use_windows) {
       graph_options.xaxis.min = this.rrdflot_defaults.window_min;  
       graph_options.xaxis.max = this.rrdflot_defaults.window_max;  
    } else {
    graph_options.xaxis.min=selection_range.xaxis.from;
    graph_options.xaxis.max=selection_range.xaxis.to;
    }
  } else if(this.rrdflot_defaults.use_windows) {
    graph_options.xaxis.min = this.rrdflot_defaults.window_min;  
    graph_options.xaxis.max = this.rrdflot_defaults.window_max;  
  } else {
    graph_options.xaxis.min=flot_obj.min;
    graph_options.xaxis.max=flot_obj.max;
  }

  var scale_options = {
    legend: {show:false},
    lines: {show:true},
    xaxis: { mode: "time", min:flot_obj.min, max:flot_obj.max },
    selection: { mode: "x" },
  };
    
  var flot_data=flot_obj.data;

  var graph_data=this.selection_range.trim_flot_data(flot_data);
  var scale_data=flot_data;

  this.graph = $.plot($(graph_jq_id), graph_data, graph_options);
  this.scale = $.plot($(scale_jq_id), scale_data, scale_options);

  if(this.rrdflot_defaults.use_windows) {
    ranges = {};
    ranges.xaxis = [];
    ranges.xaxis.from = this.rrdflot_defaults.window_min;
    ranges.xaxis.to = this.rrdflot_defaults.window_max;
    rf_this.scale.setSelection(ranges,true);
    window_min = ranges.xaxis.from;
    window_max = ranges.xaxis.to;
  }

  if (this.selection_range.isSet()) {
    this.scale.setSelection(this.selection_range.getFlotRanges(),true); //don't fire event, no need
  }

  // now connect the two    
  $(graph_jq_id).bind("plotselected", function (event, ranges) {
      // do the zooming
      rf_this.selection_range.setFromFlotRanges(ranges);
      graph_options.xaxis.min=ranges.xaxis.from;
      graph_options.xaxis.max=ranges.xaxis.to;
      window_min = ranges.xaxis.from;
      window_max = ranges.xaxis.to;
      rf_this.graph = $.plot($(graph_jq_id), rf_this.selection_range.trim_flot_data(flot_data), graph_options);
      
      // don't fire event on the scale to prevent eternal loop
      rf_this.scale.setSelection(ranges, true);
  });
    
  $(scale_jq_id).bind("plotselected", function (event, ranges) {
      rf_this.graph.setSelection(ranges);
  });

  // only the scale has a selection
  // so when that is cleared, redraw also the graph
  $(scale_jq_id).bind("plotunselected", function() {
      rf_this.selection_range.reset();
      graph_options.xaxis.min=flot_obj.min;
      graph_options.xaxis.max=flot_obj.max;
      rf_this.graph = $.plot($(graph_jq_id), rf_this.selection_range.trim_flot_data(flot_data), graph_options);
  });
};

// callback functions that are called when one of the selections changes
rrdFlotMatrix.prototype.callback_res_changed = function() {
  this.drawFlotGraph();
};

rrdFlotMatrix.prototype.callback_ds_changed = function() {
  this.drawFlotGraph();
};

rrdFlotMatrix.prototype.callback_rrd_cb_changed = function() {
  this.drawFlotGraph();
};

rrdFlotMatrix.prototype.callback_scale_reset = function() {
  this.scale.clearSelection();
};

rrdFlotMatrix.prototype.callback_legend_changed = function() {
  this.drawFlotGraph();
};

rrdFlotMatrix.prototype.callback_elem_group_changed = function(num) { 

  var oCB=document.getElementById(this.rrd_cb_id);
  var nrRRDs=oCB.rrd.length;
  if (oCB.rrd.length>0) {
    for (var i=0; i<oCB.rrd.length; i++) {
      if(Math.floor(i/this.rrdflot_defaults.num_cb_rows)==num-1) {oCB.rrd[i].checked=true; }
      else {oCB.rrd[i].checked=false;}
    }
  }
  this.drawFlotGraph()
};

function populateGraphOptions(me, other) {
  for (e in other) {
    if (me[e]!=undefined) {
      if (Object.prototype.toString.call(other[e])=="[object Object]") {
	me[e]=populateGraphOptions(me[e],other[e]);
      } else {
	me[e]=other[e];
      }
    } else {
      /// create a new one
      if (Object.prototype.toString.call(other[e])=="[object Object]") {
	// This will do a deep copy
	me[e]=populateGraphOptions({},other[e]);
      } else {
	me[e]=other[e];
      }
    }
  }
  return me;
};
