inputFilename = getArgument();

// This file performs PIV on the raw data exported from the lif files. See the
// directory structure expectations described in the documentation.

input = File.openAsString(inputFilename);
filesList = split(input, "\n");

dataDir = replace(inputFilename, "[^/]+$", "");

// Extract parameters from the experiment file.
if(isOpen("Log")) { // Close log.
	selectWindow("Log");
	run("Close");
}
file_experiment = dataDir+"Experiment.txt";
experiment_file = File.openAsString(file_experiment);
print(experiment_file);
experiment_file = split(experiment_file, "\n");

rdDir = "";
dDir = "";
ch_Phase = 1;
interval = 1;
width = 1392;
height = 1040;
for(row=0;row<experiment_file.length;row++){
	terminus = "";
	file_row = experiment_file[row];
	if(matches(file_row, ".+\\]\\: .+")){
		terminus = split(file_row, "\] ");
		terminus = terminus[terminus.length - 1];
	}
	if(matches(file_row, ".+rawdataDir\\].+")){rdDir=terminus;}
	if(matches(file_row, ".+dataDir\\].+")){dDir=terminus;}
	if(matches(file_row, ".+ch_Phase\\].+")){ch_Phase=parseInt(terminus);}
	if(matches(file_row, ".+interval\\].+")){interval=parseInt(terminus);}
	if(matches(file_row, ".+width\\].+")){width=parseInt(terminus);}
	if(matches(file_row, ".+height\\].+")){height=parseInt(terminus);}
}

print("\n");print("rdDir: "+rdDir);
print("\n");print("dDir: "+dDir);
print("\n");print("ch_Phase: "+ch_Phase);
print("\n");print("interval: "+interval);
print("\n");print("width: "+width);
print("\n");print("height: "+height);

// Iterate through all open stacks.
for(fileNumber=0;fileNumber<filesList.length;fileNumber++){
	path_raw = filesList[fileNumber]+"/";
	path_out = dDir+replace(filesList[fileNumber], rdDir, '');
	File.makeDirectory(path_out);
	path_out = path_out+"/";

	run("Image Sequence...", "open="+path_raw+" file=_ch00 sort use");
	rename("1.tif");
	run("Make Composite", "display=Grayscale");
	run("Image Sequence...", "open="+path_raw+" file=_ch01 sort use");
	rename("2.tif");
	run("Make Composite", "display=Grayscale");

	run("Concatenate...", "  title=[Concatenated Stacks] image1=1.tif image2=2.tif image3=[-- None --]");
	run("Re-order Hyperstack ...", "channels=[Frames (t)] slices=[Slices (z)] frames=[Channels (c)]");
	Stack.setDisplayMode("color");
	run("Grays");
	Stack.setDisplayMode("composite");

	// Save the raw stack.
	saveAs("Tiff", path_out+"raw.tif");

	// Perform PIV on the phase channel of the stack.
	selectWindow("raw.tif");
	run("Duplicate...", "title=piv.tif duplicate channels="+ch_Phase);
	selectWindow("raw.tif");
	close();
	selectWindow("piv.tif");
	run("Grays");

	// Convert Hyperstack to Stack
	run("Hyperstack to Stack");

	num_frames = nSlices();
	//num_frames = 3;// TODO: Remove this line.
	for(i=2;i<=num_frames;i++){
		// Set current and past frame numbers. Swap the time points for the PIV to
		// point in the correct direction.
		c_frame = i;
		p_frame = i-1;

		run("Duplicate...", "duplicate range="+p_frame+"-"+c_frame+"");
		run("Reverse");

		// Perform PIV.
		piv_text_fname = path_out+"PIV_t"+c_frame+".txt";
		piv_scale_fname = path_out+"PIV_scale.tif";
		piv_vec_fname = path_out+"PIV_vec_t"+c_frame+".tif";
		piv_mag_fname = path_out+"PIV_mag_t"+c_frame+".tif";
		//run("iterative PIV(Basic)...", "piv1=256 sw1=512 piv2=128 sw2=256 piv3=64 sw3=128 correlation=0.60 what=[Accept this PIV and output] noise=0.20 threshold=5 c1=3 c2=1 save="+piv_text_fname);
		run("iterative PIV(Basic)...", "piv1=128 sw1=256 piv2=32 sw2=64 piv3=16 sw3=32 correlation=0.99 what=[Accept this PIV and output] noise=2 threshold=20 c1=3 c2=1 save="+piv_text_fname);
		close();
		close();
		close();
		close();
		close();

		// Perform plotting.
		run("plot...", "select="+piv_text_fname+" vector_scale=2 max=30 plot_width="+width+" plot_height="+height+" show draw lut=S_Pet");
		if(i==2){// Only bother to save the scale if this is the first frame set analyzed.
			run("Save", "save="+piv_scale_fname);
		}
		close();
		run("Save", "save="+piv_vec_fname);
		close();
		run("RGB Color");
		run("Save", "save="+piv_mag_fname);
		close();

	}

	// Merge output tif files.
	piv_vecMerge_fname = path_out+"PIV_vec.tif";
	run("Image Sequence...", "open="+piv_vec_fname+" number="+num_frames-1+" file=PIV_vec_t sort use");
	saveAs("Tiff", piv_vecMerge_fname);
	close();

	piv_magMerge_fname = path_out+"PIV_mag.tif";
	run("Image Sequence...", "open="+piv_mag_fname+" number="+num_frames-1+" file=PIV_mag_t sort use");
	saveAs("Tiff", piv_magMerge_fname);
	close();

	// Clean up.
	if(isOpen("Log")) { // Close log.
		selectWindow("Log");
		run("Close");
	}
	// Print a file header.
	piv_data_fname = path_out+"PIV.txt";
	piv_file = File.open(piv_data_fname);
	print(piv_file, "frame,time,x,y,ux1,uy1,p1");
	for(i=2;i<=num_frames;i++){
		c_frame = i;

		// Delete solo images.
		vec_del = path_out+"PIV_vec_t"+c_frame+".tif";
		mag_del = path_out+"PIV_mag_t"+c_frame+".tif";

		File.delete(vec_del);
		File.delete(mag_del);

		// Collect PIV data.
		c_piv_fname = path_out+"PIV_t"+c_frame+".txt";// Get current file name.
		piv_string = File.openAsString(c_piv_fname);// Open that file.
		rows = split(piv_string, "\n");// Divide the data into rows.
		for(row=0;row<rows.length;row++){// Iterate through rows and retrieve values.
			cols = split(rows[row], " ");// Split the row into columns.
			print(piv_file, i+","+(i-1)*interval+","+cols[0]+","+cols[1]+","+cols[2]+","+cols[3]+","+cols[6]);
		}
		if(isOpen("Log")) { // Close log.
			selectWindow("Log");
			run("Close");
		}
		File.delete(c_piv_fname);
	}

	File.close(piv_file);

	close();

}
