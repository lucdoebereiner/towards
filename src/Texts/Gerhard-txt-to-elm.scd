(
var numLines, numChars;
var cwd, towards, emptyLine, paragraphs, numParagraphs;
var header, file, insertGridComment, padding, lineNum;

numLines = 30;
numChars = 40;

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

lineNum = 0;
paragraphs.do({ |lines, pageNum|
	var prefix;

	prefix = (pageNum == 0).if({ "    [ ( " }, { "    , ( " });
	(pageNum < 10).if({ prefix = prefix ++ " " });

	file.write(prefix);
	file.write(pageNum.asString);
	file.write(", Texts.noNl \"");

	(lineNum >= numLines).if({ lineNum = 0 });
	lineNum.do({ file.write(emptyLine) });

	lines.do({ |line|
		file.write(line);
		lineNum = lineNum + 1;
	});

	(numLines - lineNum).do({ file.write(emptyLine) });

	file.write("\" )\n");
	insertGridComment.();
});

file.write("    ]\n");
file.close();

)
