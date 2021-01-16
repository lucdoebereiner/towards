
// import { BufferLoader } from 'js/bufferloader.js'; // doesn't work : hate

class BufferLoader {

    constructor (context, node, callback) {
	this.context = context;
	this.node = node;
	this.onload = callback;
	this.bufferList = [];
	this.loadCount = 0;
	this.names = ["david", "gerhard", "luc", "ludvig"];
    }

    playBuffer(page, index){
	// console.log([page, index, this.bufferList[page][index]]);
	this.node.port.postMessage({load: [index, this.bufferList[page][index]]});
    }
    
    loadBuffer(page, index, firstCall) {
	// Load buffer asynchronously
	var request = new XMLHttpRequest();
	var url = "js/page" + page + "/" + this.names[index] + ".wav";
	
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
		    loader.bufferList[page][index] = [];
		    loader.bufferList[page][index][0] = buffer.getChannelData(0);
		    loader.bufferList[page][index][1] = buffer.getChannelData(1);
		    if (firstCall) {
			loader.playBuffer(page, index);
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

    load(page, firstCall) {
	if (!this.bufferList[page]) {
	    console.log("page not yet loaded");
	    this.bufferList[page] = [];
	    for (var i = 0; i < 4; ++i) {
		this.loadBuffer(page, i, firstCall);
	    }
	} else {
	    console.log("page already loaded :: nothing to be done");
	}	    
    }
}


const initButton = document.querySelector('button');

const play00 = document.querySelector('#p00');
const play10 = document.querySelector('#p10');
const play20 = document.querySelector('#p20');
const play30 = document.querySelector('#p30');

const play01 = document.querySelector('#p01');
const play11 = document.querySelector('#p11');
const play21 = document.querySelector('#p21');
const play31 = document.querySelector('#p31');

const play02 = document.querySelector('#p02');
const play12 = document.querySelector('#p12');
const play22 = document.querySelector('#p22');
const play32 = document.querySelector('#p32');

const play03 = document.querySelector('#p03');
const play13 = document.querySelector('#p13');
const play23 = document.querySelector('#p23');
const play33 = document.querySelector('#p33');


// Create AudioContext and source
let audioCtx;
let source;
let bufferLoader;

async function init() {
    
    if (navigator.mediaDevices) {
        navigator.mediaDevices.getUserMedia ({audio: true, video: false})
            .then(async function(stream) {
		
                audioCtx = new AudioContext();                      
                source = audioCtx.createMediaStreamSource(stream);
                
                await audioCtx.audioWorklet.addModule('js/playbuf.js');
		
                const playbuf = new AudioWorkletNode(audioCtx,'playbufprocessor');

		source.connect(playbuf); 
                playbuf.connect(audioCtx.destination);
		
		bufferLoader = new BufferLoader(
		    audioCtx,
		    playbuf
		);
		
		bufferLoader.load(0, true); // load all audio files for page 0 and play
		bufferLoader.load(1); // load page 1
		bufferLoader.load(2); // load page 1
		bufferLoader.load(3); // load page 1
            })
    }

}

// wire up init and play buttons
initButton.onclick = function() {
    if(!audioCtx) {
        init();
    }
}

play00.onclick = function() {
    console.log("play page 0 idx 0");
    bufferLoader.playBuffer(0, 0);
}
play10.onclick = function() {
    console.log("play page 1 idx 0");
    bufferLoader.playBuffer(0, 1);
}
play20.onclick = function() {
    console.log("play page 2 idx 0");
    bufferLoader.playBuffer(0, 2);
}
play30.onclick = function() {
    console.log("play page 3 idx 0");
    bufferLoader.playBuffer(0, 3);
}

play01.onclick = function() {
    console.log("play page 0 idx 1");
    bufferLoader.playBuffer(1, 0);
}
play11.onclick = function() {
    console.log("play page 1 idx 1");
    bufferLoader.playBuffer(1, 1);
}
play21.onclick = function() {
    console.log("play page 2 idx 1");
    bufferLoader.playBuffer(1, 2);
}
play31.onclick = function() {
    console.log("play page 3 idx 1");
    bufferLoader.playBuffer(1, 3);
}

play02.onclick = function() {
    console.log("play page 0 idx 2");
    bufferLoader.playBuffer(2, 0);
}
play12.onclick = function() {
    console.log("play page 1 idx 2");
    bufferLoader.playBuffer(2, 1);
}
play22.onclick = function() {
    console.log("play page 2 idx 2");
    bufferLoader.playBuffer(2, 2);
}
play32.onclick = function() {
    console.log("play page 3 idx 2");
    bufferLoader.playBuffer(2, 3);
}

play03.onclick = function() {
    console.log("play page 0 idx 3");
    bufferLoader.playBuffer(3, 0);
}
play13.onclick = function() {
    console.log("play page 1 idx 3");
    bufferLoader.playBuffer(3, 1);
}
play23.onclick = function() {
    console.log("play page 2 idx 3");
    bufferLoader.playBuffer(3, 2);
}
play33.onclick = function() {
    console.log("play page 3 idx 3");
    bufferLoader.playBuffer(3, 3);
}

