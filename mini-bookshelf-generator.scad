// Mini Shelf (parametric, laser-cut 2D layout)
// Author: ChatGPT (FabLab helper)
// Goal: true 2D cut plans (only contours). Thickness T matters ONLY for mortise width.
// Machine bed: Flux Hexa 730 x 410 mm. We spread parts across MULTIPLE sheets.
// Included parts: 2 short sides, 2 long sides, 3 short shelves, 4 long shelves, 1 back panel.

//////////////////////////
// PARAMETERS
//////////////////////////
bed_w = 730;   // mm
bed_h = 410;   // mm
margin = 10;   // mm sheet margin
spacing = 10;  // mm spacing between parts

// Material & fit
T    = 3;       // thickness (mm)
kerf = 0.15;    // kerf compensation (mm)
fit  = 0.00;    // additional clearance (mm)
slot_w = T + kerf + fit;  // mortise cut width

// Geometry
H = 350;        // height of sides & back (mm)
D = 120;        // depth (mm) — keep ≤125 for machine clearance
W_short = 240;  // short shelf length (mm)
W_long  = 400;  // long shelf length (mm)
W_back  = W_short + W_long; // back panel width (mm)

// Mortise geometry on sides
finger_len = 12;       // mortise length (along X)
// Vertical shelf positions (centerlines from bottom inner face). Auto if arrays empty.
auto_short = true;
auto_long  = true;
short_y = [];           // ex: [60, 140, 220]
long_y  = [];           // ex: [50, 130, 210, 290]

rounded = 2;            // small corner radius

//////////////////////////
// HELPERS
//////////////////////////
module rrect(w,h,r=0){
  if(r<=0) square([w,h], center=false);
  else offset(r=r) offset(delta=-r) square([w,h], center=false);
}
module mortise(cx, cy, L, dir="x"){  // rectangle centered at (cx,cy)
  w = slot_w;
  if (dir=="x") translate([cx - L/2, cy - w/2]) square([L, w]);
  else          translate([cx - w/2, cy - L/2]) square([w, L]);
}
inner_H = H - 2*T;
list_auto = function(n) let(step = inner_H/(n+1)) [ for(i=[1:n]) i*step ];

//////////////////////////
// PARTS (2D OUTLINES ONLY)
//////////////////////////
module side_panel(tag="short"){
  ys = (tag=="short") ? (auto_short ? list_auto(3) : short_y)
                       : (auto_long  ? list_auto(4) : long_y);
  n  = len(ys);
  difference(){
    rrect(D, H, rounded);
    // Two mortises per shelf (front/back)
    for(k=[0:n-1]){
      yk = T + ys[k];
      mortise(T + finger_len/2, yk, finger_len, "x");
      mortise(D - T - finger_len/2, yk, finger_len, "x");
    }
  }
}
module shelf_plate(L){ rrect(L, D, rounded); }
module back_panel(){ rrect(W_back, H, rounded); }

//////////////////////////
// MULTI-SHEET LAYOUT (fixed grouping, no rotations)
//////////////////////////
// Sheet 1: all side panels (2 short + 2 long)
module sheet1(){
  // positions
  x0 = margin; y0 = margin;
  translate([x0 + 0*(D+spacing), y0]) side_panel("short");
  translate([x0 + 1*(D+spacing), y0]) side_panel("short");
  translate([x0 + 2*(D+spacing), y0]) side_panel("long");
  translate([x0 + 3*(D+spacing), y0]) side_panel("long");
}

// Sheet 2: two long shelves (no rotation)
module sheet2(){
  x0 = margin; y0 = margin;
  translate([x0, y0 + 0*(D+spacing)]) shelf_plate(W_long);
  translate([x0, y0 + 1*(D+spacing)]) shelf_plate(W_long);
}

// Sheet 3: two long shelves
module sheet3(){
  x0 = margin; y0 = margin;
  translate([x0, y0 + 0*(D+spacing)]) shelf_plate(W_long);
  translate([x0, y0 + 1*(D+spacing)]) shelf_plate(W_long);
}

// Sheet 4: three short shelves
module sheet4(){
  x0 = margin; y0 = margin;
  translate([x0, y0 + 0*(D+spacing)]) shelf_plate(W_short);
  translate([x0, y0 + 1*(D+spacing)]) shelf_plate(W_short);
  translate([x0, y0 + 2*(D+spacing)]) shelf_plate(W_short);
}

// Sheet 5: back panel
module sheet5(){
  translate([margin, margin]) back_panel();
}

//////////////////////////
// RENDER SHEETS
//////////////////////////
module bed(){ color([0,0,0,0.05]) square([bed_w, bed_h]); }

$fn=32;
// Draw 5 sheets stacked vertically for export; you can also export each separately by hiding others.
translate([0, 0])        bed();
translate([0, 0])        color("black") sheet1();

translate([0, bed_h+20]) bed();
translate([0, bed_h+20]) color("black") sheet2();

translate([0, 2*(bed_h+20)]) bed();
translate([0, 2*(bed_h+20)]) color("black") sheet3();

translate([0, 3*(bed_h+20)]) bed();
translate([0, 3*(bed_h+20)]) color("black") sheet4();

translate([0, 4*(bed_h+20)]) bed();
translate([0, 4*(bed_h+20)]) color("black") sheet5();

// Notes:
// - Export as a single SVG (F6 → Export as SVG). Each bed rectangle frames one sheet 730x410.
// - If you want to maximize material use or change grouping, adjust sheet modules.
// - To match exact shelf heights from the PDF, set auto_short/auto_long=false and fill short_y/long_y.
