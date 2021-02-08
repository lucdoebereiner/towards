(

var numPages, secondsPerPage, workingDirectory, dayStamp, buffer, pm, am, pmam, model;

numPages = 50;
secondsPerPage = 60;
workingDirectory = thisProcess.nowExecutingPath.dirname();
dayStamp = Date.getDate().dayStamp();
buffer = Buffer.alloc(Server.default, Server.default.sampleRate * secondsPerPage);

pm = { |pmCarFreq, pmModFreq, pmIndex, modPhase = 0, min = -1, max = 1|
	PMOsc.ar(pmCarFreq, pmModFreq, pmIndex, modPhase).linlin(-1, 1, min, max);
};

am = { |amFreq, amDepth, amPhase = 0|
		var halfAmpDepth = amDepth / 2;
		DC.ar(1) - amDepth + SinOsc.ar(amFreq, amPhase, halfAmpDepth, halfAmpDepth);
};

pmam = { |pmCarFreq, pmModFreq, pmIndex, modPhase = 0, amFreq, amDepth, amPhase = 0, min = -1, max = 1|
	PMOsc.ar(pmCarFreq, pmModFreq, pmIndex, modPhase, am.(amFreq, amDepth, amPhase)).linlin(-1, 1, min, max);
};

model = { |seed = 0|
	var numComponents, randlin, randexp, roundTo, jitter;

	thisThread.randSeed = seed;

	numComponents = 2;
	randlin = { |min, max| rand(1.0).linlin(0, 1, min, max) };
	randexp = { |min, max| rand(1.0).linexp(0, 1, min, max) };
	roundTo = secondsPerPage.reciprocal();

	jitter = { |numVersions, minFactor, maxFactor|
		var pmCarFreq, pmModFreq, pmIndex, amFreq, amDepth;

		pmCarFreq = randexp.(10, 100);
		pmModFreq = randexp.(10, 100);
		pmIndex = randlin.(0, 3);
		amFreq = randexp.(1, 10);
		amDepth = rand(1.0);

		numVersions.do({ |i|
			play {
				var source, envelope;

				source = pmam.(
					(pmCarFreq * randexp.(minFactor, maxFactor)).round(roundTo),
					(pmModFreq * randexp.(minFactor, maxFactor)).round(roundTo),
					pmModFreq * randexp.(minFactor, maxFactor),
					0,
					(amFreq * randexp.(minFactor, maxFactor)).round(roundTo),
					amDepth * randexp.(minFactor, maxFactor)
				);
				source = source * (numComponents * numVersions).reciprocal();
				envelope = pm.(
					rrand(10, 50).nthPrime()/secondsPerPage,
					rrand(20, 10).nthPrime()/secondsPerPage,
					30, 0.5, 0.5, 1);
				envelope = envelope * pm.(
					rrand(1, 5).nthPrime()/secondsPerPage,
					rrand(2, 6).nthPrime()/secondsPerPage,
					10, 0, 0.5, 1);
				source  = source * envelope;
				RecordBuf.ar(source, buffer, loop:0, doneAction:Done.freeSelf);
			};
		})
	};

	numComponents.do({
		jitter.(3, 1, 1.01);
	});
};

fork {
	numPages.do({ |seed|
		var name, pagePath, filePath, linkPath, mp3Path;

		name = "gerhard";

		pagePath = workingDirectory +/+ "../files/page" ++ seed;
		filePath = pagePath +/+ name ++ "_" ++ dayStamp ++ ".wav";
		linkPath = pagePath +/+ name ++ ".wav";
		mp3Path = pagePath +/+ name ++ ".mp3";

		model.(seed);
		(buffer.duration() + 0.1).wait();

		buffer.write(filePath, "wav");
		1.wait();

		("rm" + linkPath).systemCmd();
		("ln -s" + filePath + linkPath).systemCmd();
		("ffmpeg -y -i" + linkPath + "-vn -ar 48000 -ac 1 -b:a 96k" + mp3Path).systemCmd();
	});
};

)
