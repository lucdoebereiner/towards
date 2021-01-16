

class PlayBufProcessor extends AudioWorkletProcessor {

    constructor (...args) {
	super(...args);
	this.phase = 0;
	this.length = 48000 * 60;
	this.bufswitch = Array(4).fill(0);
	this.bufs = [];
	for (var i =0; i < 4; ++i) {
	    this.bufs[i] = [];
	    this.bufs[i][0] = [];
	    this.bufs[i][1] = [];	    
	    this.bufs[i][0][0] = new Float32Array(48000 * 60);
	    this.bufs[i][0][1] = new Float32Array(48000 * 60);	    
	    this.bufs[i][1][0] = new Float32Array(48000 * 60);
	    this.bufs[i][1][1] = new Float32Array(48000 * 60);	    
	}
	
	this.port.onmessage = (e) => {
	    console.log(e.data)
	    if (e.data.load) {
		// console.log("gotload");
		// console.log(e.data.load);
		var idx = e.data.load[0];
		var sw = this.bufswitch[idx];
		sw = sw + 1;
		sw = sw % 2;
		this.bufs[idx][sw][0] = e.data.load[1][0];
		this.bufs[idx][sw][1] = e.data.load[1][1];
		this.bufswitch[idx] = sw;
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
                // output[ch][s] = ((Math.random() * 2 - 1) * 0.12);
		output[ch][s] = (this.bufs[0][this.bufswitch[0]][ch][this.phase] +
				 this.bufs[1][this.bufswitch[1]][ch][this.phase] +
				 this.bufs[2][this.bufswitch[2]][ch][this.phase] +
				 this.bufs[3][this.bufswitch[3]][ch][this.phase]) * 0.25;
            }
        }
        return true
    }
}

registerProcessor('playbufprocessor', PlayBufProcessor);
