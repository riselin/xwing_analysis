//Load Images, Make stack
//manually download and align setup images before movement
//Minimally: left/right start
//Maximally: flip horizontally and vertically, rotate to get the "highest values in the corners" or maximal overlap
run("Images to Stack", "name=Stack title=[] use");
run("8-bit");
run("Despeckle", "stack");
setAutoThreshold("Default dark");
run("Threshold...");
setOption("BlackBackground", true);
run("Convert to Mask", "method=Default background=Dark calculate black");
run("Despeckle", "stack");
run("Z Project...", "projection=[Sum Slices]");
