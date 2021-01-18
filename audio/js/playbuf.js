

class PlayBufProcessor extends AudioWorkletProcessor {

    constructor (...args) {
	super(...args);
	this.phase = 0;
	this.length = 48000 * 60;
	// this.bufswitch = Array(4).fill(0);
	this.bufamps = [];
	this.bufs = [];
	this.pageNum = 0;

	// for (var i =0; i < 4; ++i) {
	//     this.bufs[i] = [];
	//     this.bufs[i][0] = [];
	//     this.bufs[i][1] = [];	    
	//     this.bufs[i][0][0] = new Float32Array(48000 * 60);
	//     this.bufs[i][0][1] = new Float32Array(48000 * 60);	    
	//     this.bufs[i][1][0] = new Float32Array(48000 * 60);
	//     this.bufs[i][1][1] = new Float32Array(48000 * 60);	    
	// }
	
	this.port.onmessage = (e) => {
	    // console.log(e.data)
	    if (e.data.pages) {
		console.log("gotpages");
		console.log(e.data.pages);
		this.pageNum = e.data.pages;
		for (var i = 0;  i < 4; ++i) {
		    this.bufs[i] = []
		    this.bufamps[i] = [];
		    for (var j = 0;  j < this.pageNum; ++j) {
			this.bufs[i][j] = [];
			this.bufs[i][j][0] = new Float32Array(48000 * 60);
			this.bufs[i][j][1] = new Float32Array(48000 * 60);
			this.bufamps[i][j] = 0.0;
		    }
		}
		this.bufamps[0] = [1.0, 1.0, 1.0, 1.0];
	    }
	    if (e.data.load) {
		console.log("gotload");
		console.log(e.data.load);
		var pp = e.data.load[0];
		var idx = e.data.load[1];
		// var sw = this.bufswitch[idx];
		// sw = sw + 1;
		// sw = sw % 2;
		this.bufs[pp][idx][0] = e.data.load[2][0];
		this.bufs[pp][idx][1] = e.data.load[2][1];
		// this.bufswitch[idx] = sw;
	    }
	    if (e.data.amps) {
		console.log("gotamps");
		console.log(e.data.amps);
		var idx = e.data.amps[0];
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    this.bufamps[pp][idx] = e.data.amps[1][pp];
		}
	    }

	}	 
    }
    
    process (inputs, outputs, parameters) {
        const output = outputs[0];
        const input = inputs[0];
	
        for (let s = 0; s < output[0].length; s++) {
	    this.phase = this.phase + 1;
	    this.phase = this.phase % this.length;
            for (let ch = 0; ch < output.length; ch++) {
		output[ch][s] = 0.0;
		for (let pp = 0; pp<this.pageNum; ++pp) {
		    output[ch][s] = output[ch][s] +
			this.bufs[pp][0][ch][this.phase] * this.bufamps[pp][0] +
			 this.bufs[pp][1][ch][this.phase] * this.bufamps[pp][1] +
			 this.bufs[pp][2][ch][this.phase] * this.bufamps[pp][2] +
			 this.bufs[pp][3][ch][this.phase] * this.bufamps[pp][3];
		}
		output[ch][s] = output[ch][s] * 0.25;
            }
        }
        return true
    }
}

registerProcessor('playbufprocessor', PlayBufProcessor);
