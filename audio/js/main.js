
// import { BufferLoader } from 'js/bufferloader.js'; // doesn't work : hate

const pageNum = 50;
const duration = 60;

class BufferLoader {

    constructor (context, node, callback) {
	this.context = context;
	this.node = node;
	this.onload = callback;
	this.loadCount = 0;
	this.names = ["david", "gerhard", "luc", "ludvig"];
	this.shBuf = [];
	this.index2l = 0;
	this.sr = this.context.sampleRate;
	this.loadIndices = [];
	// console.log("sr : ");
	// console.log(this.sr);
	this.node.port.postMessage({init: [pageNum, this.sr]});
	for (var i = 0; i < 4; ++i) {
	    this.shBuf[i] = [];
	    for (var j = 0; j < pageNum; ++j) {
		this.shBuf[i][j] = new SharedArrayBuffer(4 * this.sr * duration); // 32 bit floats
	    }
	}
	this.shArray = [];
	for (var i = 0; i < 4; ++i) {
	    this.shArray[i] = [];
	    for (var j = 0; j < pageNum; ++j) {
		this.shArray[i][j] = new Float32Array(this.shBuf[i][j]); // 32 bit floats
	    }
	}

    }

    sendBuffer(page, index){
	this.node.port.postMessage({load: [page, index, this.shBuf[index][page]]});
    }

    sendAmps(index, amps) {
	console.log(index, amps);
	this.node.port.postMessage({amps: [index, amps]});
    }

    sendPan(index, pan) {
	this.node.port.postMessage({pan: [index, pan]});
    }

    loadBuffer( ) {
	// Load buffer asynchronously
	var request = new XMLHttpRequest();
	var index = Math.floor((this.index2l%8)/2);
	var page = this.loadIndices[ this.index2l ];
	var url = "audio/files/page" + page + "/" + this.names[index] + ".mp3";

	console.log(url);

	request.open("GET", url, true);
	request.responseType = "arraybuffer";

	var loader = this;

	request.onload = function() {
	    loader.context.decodeAudioData(
		request.response,
		function(buffer) {
		    if (!buffer) {
			alert('error decoding file data: ' + url);
			return;
		    }
		    var chdata = buffer.getChannelData(0);
		    for (var i = 0; i<loader.sr * duration; ++i) {
			loader.shArray[index][page][i] = chdata[i];
		    };
		    loader.index2l = loader.index2l + 1;
		    if (loader.index2l%8 == 0) {
			for (var j = 1; j<9; ++j) {
			    var pi = Math.floor(((loader.index2l - j)%8)/2);
			    var pp = loader.loadIndices[ loader.index2l - j ];
			    loader.sendBuffer(pp, pi);
			}
		    }
		    if (loader.index2l < pageNum*4) {
			// console.log("calling next");
			loader.loadBuffer( );
		    }
		},
		function(error) {
		    console.error('decodeAudioData error', error);
		}
	    );
	}

	request.onerror = function() {
	    alert('BufferLoader: XHR error');
	}

	request.send();
    }

    loadAll(indices) {
	var f = []; 	
	for (var i = 0; i<4; ++i) {
	    f[i] = Math.floor(indices[i]);
	};
	for (var j = 0; j<25; ++j) {
	    for (var i = 0; i<4; ++i) {
		this.loadIndices.push((f[i] - j + 50)%50);
		this.loadIndices.push((f[i] + j + 1)%50);
	    };
	}
	this.loadBuffer( );
    }
}


// const initButton = document.querySelector('button');

// const fad0 = document.getElementById("fader0");
// const fad1 = document.getElementById("fader1");
// const fad2 = document.getElementById("fader2");
// const fad3 = document.getElementById("fader3");

// Create AudioContext and source
let audioCtx;
let source;
let bufferLoader;

async function init(indices, elmInitCallback) {


    console.log(indices);
    
    audioCtx = new AudioContext();
    // source = audioCtx.createMediaStreamSource(stream);

    await audioCtx.audioWorklet.addModule('audio/js/playbuf.js');

    const playbuf = new AudioWorkletNode(audioCtx,'playbufprocessor', {
	outputChannelCount: [2],
    });
    playbuf.connect(audioCtx.destination);


    bufferLoader = new BufferLoader(
	audioCtx,
	playbuf
    );

    elmInitCallback.send(true);

    bufferLoader.loadAll(indices);

}

// // wire up init and play buttons
// initButton.onclick = function() {
//     if(!audioCtx) {
//         init();
//     }
// }


// faderToAmp = function(idx, value) {
//     var ta = new Float32Array(pageNum);
//     var v = value;
//     var fv = Math.floor(value);
//     var cv = fv + 1;
//     ta[fv] = 1.0 - (v - fv);
//     if (cv < pageNum) {
// 	ta[cv] = v - fv;
//     }
//     bufferLoader.sendAmps(idx, ta)
// }

// fad0.oninput = function() {
//     console.log("got fader0");
//     console.log(this.value);
//     document.querySelector('#volume0').value = this.value;
//     faderToAmp(0, this.value);
// }
// fad1.oninput = function() {
//     console.log("got fader1");
//     console.log(this.value);
//     document.querySelector('#volume1').value = this.value;
//     faderToAmp(1, this.value);
// }
// fad2.oninput = function() {
//     console.log("got fader2");
//     console.log(this.value);
//     document.querySelector('#volume2').value = this.value;
//     faderToAmp(2, this.value);
// }
// fad3.oninput = function() {
//     console.log("got fader3");
//     console.log(this.value);
//     document.querySelector('#volume3').value = this.value;
//     faderToAmp(3, this.value);
// }
