

class PlayBufProcessor extends AudioWorkletProcessor {

    constructor (...args) {
	super(...args);
	this.phase = 0;
	this.length = 48000 * 60;
	this.bufamps = [];
	this.pageNum = 0;
	this.counter = 0;
	this.shArray = [];
	this.init = false;
	
	this.port.onmessage = (e) => {
	    if (e.data.init) {
		// console.log("gotinit");
		// console.log(e.data.init);
		this.pageNum = e.data.init;
		for (var i = 0;  i < 4; ++i) {
		    this.shArray[i] = [];
		    this.bufamps[i] = [];
		    for (var j = 0;  j < this.pageNum; ++j) {
			this.shArray[i][j] = new Float32Array(48000 * 60);
			this.bufamps[i][j] = 0.0;
		    }
		}
		this.bufamps[0][0] = 1.0;
		this.bufamps[1][0] = 1.0;
		this.bufamps[2][0] = 1.0;
		this.bufamps[3][0] = 1.0;
		this.init = true;
	    }
	    if (e.data.load) {
		// console.log("gotload");
		// console.log(e.data.load);
		var pp = e.data.load[0];
		var idx = e.data.load[1];
		this.shArray[idx][pp] = new Float32Array(e.data.load[2]);
	    }
	    if (e.data.amps) {
		// console.log("gotamps");
		// console.log(e.data.amps);
		var idx = e.data.amps[0];
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    this.bufamps[idx][pp] = e.data.amps[1][pp];
		}	
	    }

	}	 
    }
    
    process (inputs, outputs, parameters) {
        const output = outputs[0];
        const input = inputs[0];
	if (this.init) {
            for (let s = 0; s < output[0].length; s++) {
		let so = 0.0;
		this.phase = this.phase + 1;
		this.phase = this.phase % this.length;
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    so = so +
			this.shArray[0][pp][this.phase] * this.bufamps[0][pp] +
			this.shArray[1][pp][this.phase] * this.bufamps[1][pp] +
			this.shArray[2][pp][this.phase] * this.bufamps[2][pp] +
			this.shArray[3][pp][this.phase] * this.bufamps[3][pp];
		}
		output[0][s] = so * 0.5;
		output[1][s] = so * 0.5;
		// if (this.counter > 4096) { // for debugging 
		//     // console.log(so);
		//     // // console.log(this.bufs);
		//     // console.log(this.bufamps);
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
