(
var numPages, numLines, numChars;
var cwd, towards, emptyLine, paragraphs, numParagraphs, paraSpacing;
var header, file, insertGridComment, padding, lineNum, pageNum, paraLine;
var writeBeginOfLine, writeEndOfLine, incrementLineNum, incrementLineNumAndParaLine;

numPages = 50;
numLines = 30;
numChars = 40;
paraSpacing = 0;

cwd = thisProcess.nowExecutingPath.dirname();
towards = FileReader.read(cwd +/+ "Gerhard.txt");
towards = towards.collect({|line| line.join(Char.space()) });
emptyLine = towards[2];

paragraphs = [[]];
numParagraphs = 0;
towards.do({ |line|
	(line == emptyLine).if({
		numParagraphs = numParagraphs + 1;
		paragraphs = paragraphs.add([]);
	}, {
		paragraphs[numParagraphs] = paragraphs[numParagraphs].add(line);
	});
});

header = "module Texts.Gerhard exposing (texts)\n\n";
header = header ++ "import Texts\n\n\n";
header = header ++ "texts : List Texts.EntryWithIndex\n";
header = header ++ "texts =\n";

file = File.open(cwd +/+ "Gerhard.elm", "w");
file.write(header);

insertGridComment = {
	file.write("---                     ");
	padding = Array.fill(numChars - 2, " ").join();
	numLines.do({ |index|
		var number;

		number = (index < 10).if({ "0" ++ index.asString() }, { index.asString() });
		file.write(number);
		file.write(padding);
	});
	file.write("END\n");
};

insertGridComment.();

pageNum = 0;
lineNum = 0;
paraLine = 0;

writeBeginOfLine = { |pageNum|
	var begin;

	begin = (pageNum == 0).if({ "    [ ( " }, { "    , ( " });
	(pageNum < 10).if({ begin = begin ++ " " });
	file.write(begin);
	file.write(pageNum.asString);
	file.write(", Texts.noNl \"");
};

writeEndOfLine = {
	file.write("\" )\n");
	insertGridComment.();
};

incrementLineNum = {
	lineNum = lineNum + 1;
	(lineNum >= numLines).if({
		lineNum = 0;
		pageNum = pageNum + 1;
		writeEndOfLine.();
		(pageNum < numPages).if({
			writeBeginOfLine.(pageNum);
		});
	});
};

incrementLineNumAndParaLine = {
	incrementLineNum.();
	paraLine = paraLine + 1;
	((lineNum + 3) > numLines).if({
		paraLine = 0;
	});
};

writeBeginOfLine.(pageNum);

paragraphs.do({ |lines, numPara|

	paraLine.do({
		file.write(emptyLine);
		incrementLineNum.();
	});

	lines.do({ |line|
		file.write(line);
		incrementLineNumAndParaLine.();
	});

	((lineNum + 2) > numLines).if({
		paraLine = 0;
	}, {
		paraSpacing.do({
			file.write(emptyLine);
			incrementLineNumAndParaLine.();
		});
	});

	(numLines - lineNum).do({
		file.write(emptyLine);
		incrementLineNum.();
	});

});

(numPages - pageNum).do({
	numLines.do({
		file.write(emptyLine);
		incrementLineNum.();
	});
});

file.write("    ]\n");
file.close();

)
