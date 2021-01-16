
// bufferloader class

export class BufferLoader {

    constructor (context, urlList, callback) {
	this.context = context;
	this.urlList = urlList;
	this.onload = callback;
	this.bufferList = new Array();
	this.loadCount = 0;
	console.log("contructor!!");
	console.log(this.context);
    }

    loadBuffer(url, index) {
	// Load buffer asynchronously
	var request = new XMLHttpRequest();
	request.open("GET", url, true);
	request.responseType = "arraybuffer";

	console.log("loadbuffer!!");
	console.log(this.context);

	var loader = this;
	
	request.onload = function() {
	    // Asynchronously decode the audio file data in request.response
	    console.log("onload!!");
	    console.log(loader.loadCount);
	    console.log(loader.context);
	    loader.context.decodeAudioData(
		request.response,
		function(buffer) {
		    if (!buffer) {
			alert('error decoding file data: ' + url);
			return;
		    }
		    loader.bufferList[index] = buffer;
		    if (++loader.loadCount == loader.urlList.length)
			loader.onload(loader.bufferList);
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

    load() {
	for (var i = 0; i < this.urlList.length; ++i)
	    this.loadBuffer(this.urlList[i], i);
    }
}
