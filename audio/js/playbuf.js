class PlayBufProcessor extends AudioWorkletProcessor {

    constructor (...args) {
	super(...args);
	this.phase = 0;
	this.sr = 48000;
	this.duration = 60;
	this.length = this.sr * this.duration;
	this.amps = [];
	this.pan = [[0.9, 0.1], [0.65, 0.35], [0.35, 0.65], [0.1, 0.9]];
	this.pageNum = 0;
	this.counter = 0;
	this.shArray = [];
	this.init = false;
	this.on = [true, true, true, true];
	// console.log("numout, ch");
	// console.log([this.numberOfOutputs]);
	
	this.port.onmessage = (e) => {
	    if (e.data.init) {
		// console.log("gotinit");
		// console.log(e.data.init);
		this.pageNum = e.data.init[0];
		this.sr = e.data.init[1];
		this.duration = e.data.init[2];
		this.length = this.sr * this.duration;
		for (var i = 0;  i < 4; ++i) {
		    this.shArray[i] = [];
		    this.amps[i] = [];
		    for (var j = 0;  j < this.pageNum; ++j) {
			this.shArray[i][j] = new Float32Array(this.sr * this.duration);
			this.amps[i][j] = 0.0;
		    }
		}
		this.amps[0][0] = 1.0;
		this.amps[1][0] = 1.0;
		this.amps[2][0] = 1.0;
		this.amps[3][0] = 1.0;
		this.init = true;
	    }
	    if (e.data.load) {
		console.log("gotload");
		console.log(e.data.load);
		var pp = e.data.load[0];
		var idx = e.data.load[1];
		this.shArray[idx][pp] = e.data.load[2];
	    }
	    if (e.data.amps) {
		// console.log("gotamps");
		// console.log(e.data.amps);
		var idx = e.data.amps[0];
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    this.amps[idx][pp] = e.data.amps[1][pp];
		}
	    }
	    if (e.data.pan) {
		// console.log("gotpan");
		// console.log(e.data.pan);
		var idx = e.data.pan[0];
		var pan = e.data.pan[1];
		this.pan[idx] = [(1.0 - pan) * 0.5, (pan + 1) * 0.5];
	    }
	    if (e.data.mute) {
		console.log("gotmute");
		console.log(e.data.mute);
		var idx = e.data.mute[0];
		var monoff = e.data.mute[1];
		this.on[idx] = !monoff;
	    }
	}
    }

    process (inputs, outputs, parameters) {
	// console.log("called process");
        const output = outputs[0];
        // const input = inputs[0];
	// console.log(["outputs : ", output]);
	// console.log(this.pan);
//	console.log(this.on);
	if (this.init) {
            for (let s = 0; s < output[0].length; s++) {
		let so = [0.0, 0.0, 0.0, 0.0];
		this.phase = this.phase + 1;
		this.phase = this.phase % this.length;
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    if (this.on[0]) {
			so[0] = so[0] + this.shArray[0][pp][this.phase] * this.amps[0][pp];
		    };
		    if (this.on[1]) {
			so[1] = so[1] + this.shArray[1][pp][this.phase] * this.amps[1][pp];
		    }
		    if (this.on[2]) {
			so[2] = so[2] + this.shArray[2][pp][this.phase] * this.amps[2][pp];
		    }
		    if (this.on[3]) {
			so[3] = so[3] + this.shArray[3][pp][this.phase] * this.amps[3][pp];
		    }
		}
		output[0][s] = so[0] * this.pan[0][0] +
		    so[1] * this.pan[1][0] +
		    so[2] * this.pan[2][0] +
		    so[3] * this.pan[3][0];
		output[1][s] = so[0] * this.pan[0][1] +
		    so[1] * this.pan[1][1] +
		    so[2] * this.pan[2][1] +
		    so[3] * this.pan[3][1];
		// output[1][s] = 0.0;
		// if (this.counter > 4096) { // for debugging
		//     // console.log(so);
		//     // // console.log(this.bufs);
		//     // console.log(this.amps);
		//     // console.log(this.shArray);
		//     this.counter = 0;
		// } else {
		//     this.counter = this.counter + 1;
		// }
            }
	}
        return true
    }
}

registerProcessor('playbufprocessor', PlayBufProcessor);
