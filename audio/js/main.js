
// import { BufferLoader } from 'js/bufferloader.js'; // doesn't work : hate

const pageNum = 4;

class BufferLoader {

    constructor (context, node, callback) {
	this.context = context;
	this.node = node;
	this.onload = callback;
	this.bufferList = [];
	this.loadCount = 0;
	this.names = ["david", "gerhard", "luc", "ludvig"];
    }

    // playBuffer(page, index){
    // 	// console.log([page, index, this.bufferList[page][index]]);
    // 	this.node.port.postMessage({load: [index, this.bufferList[page][index]]});
    // }

    sendBuffer(page, index){
	// console.log([page, index, this.bufferList[page][index]]);
	this.node.port.postMessage({load: [page, index, this.bufferList[page][index]]});
    }

    sendAmps(index, amps) {
	this.node.port.postMessage({amps: [index, amps]});
    }
    
    loadBuffer(page, index, firstCall) {
	// Load buffer asynchronously
	var request = new XMLHttpRequest();
	var url = "files/page" + page + "/" + this.names[index] + ".mp3";

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
		    loader.bufferList[page][index] = [];
		    loader.bufferList[page][index][0] = buffer.getChannelData(0);
		    loader.bufferList[page][index][1] = buffer.getChannelData(1);
		    // if (firstCall) {
		    // 	loader.playBuffer(page, index);
		    // }
		    loader.sendBuffer(page, index);
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

    loadAll( ) {
	if (this.bufferList[page]) {
	    alert('erro : buffer list is not empty');
	} else {
	    console.log("not yet loaded");
	    this.node.port.postMessage({pages: pageNum});
	    for (var page = 0; page < pageNum; ++page) {
		this.bufferList[page] = [];
		for (var i = 0; i < 4; ++i) {
		    this.loadBuffer(page, i);
		}
	    }
	} 	    
    }
}


const initButton = document.querySelector('button');

const fad0 = document.getElementById("fader0");
const fad1 = document.getElementById("fader1");
const fad2 = document.getElementById("fader2");
const fad3 = document.getElementById("fader3");

// Create AudioContext and source
let audioCtx;
let source;
let bufferLoader;

async function init() {

    console.log(fad0);
    
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

		bufferLoader.loadAll( ); // load all audio files 0 and play page 0
		
            })
    }

}

// wire up init and play buttons
initButton.onclick = function() {
    if(!audioCtx) {
        init();
    }
}


faderToAmp = function(idx, value) {
    var ta = [0.0, 0.0, 0.0, 0.0];
    var v = value;
    var fv = Math.floor(value);
    var cv = fv + 1;
    ta[fv] = 1.0 - (v - fv);
    if (cv < pageNum) {
	ta[cv] = v - fv;
    }
    bufferLoader.sendAmps(idx, ta)
}

fad0.oninput = function() {
    console.log("got fader0");
    console.log(this.value);
    document.querySelector('#volume0').value = this.value;
    faderToAmp(0, this.value);
}
fad1.oninput = function() {
    console.log("got fader1");
    console.log(this.value);
    document.querySelector('#volume1').value = this.value;
    faderToAmp(1, this.value);
}
fad2.oninput = function() {
    console.log("got fader2");
    console.log(this.value);
    document.querySelector('#volume2').value = this.value;
    faderToAmp(2, this.value);
}
fad3.oninput = function() {
    console.log("got fader3");
    console.log(this.value);
    document.querySelector('#volume3').value = this.value;
    faderToAmp(3, this.value);
}

