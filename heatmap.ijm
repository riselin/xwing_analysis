//Load Images, Make stack
run("Images to Stack", "name=Stack title=[] use");
run("8-bit");
run("Despeckle", "stack");
setAutoThreshold("Default dark");
run("Threshold...");
setOption("BlackBackground", true);
run("Convert to Mask", "method=Default background=Dark calculate black");
run("Despeckle", "stack");
run("Z Project...", "projection=[Sum Slices]");
