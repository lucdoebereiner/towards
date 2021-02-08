
~d = ["do", "doing", "done", "undone", "undo", "undoing"];
~t = ["think", "thinking", "thought", "unthinking", "unthought", "unthink"];
~c = ["up", "down", "through", "about", "in", "out", "right", "wrong", "concrete", "abstract", "hard", "soft", "carefully", "recklessly", "over", "under", "much", "little", "beside", "between", "together", "positive", "negative", "clear", "ambiguous", "loud", "quiet", "now", "later", "yes", "no", "around", "clean", "dirty", "wild", "refined", "actual", "uncertain", "imprecise", "thorough", "big", "small", "hideous", "beautiful"];
~p = ["·", "-", "\\", "/", "|"];
~pt = [[0, 1, 2, 3, 4], [0, 1, 2, 3], [0, 1, 4], [0, 1, 4], [2, 3]];

~pageNum = 50;
~lineNum = 30;
~charNum = 40;

// ~pages = Array.fill(~pageNum, Array.fill(~lineNum, Array,fill(~charNum, ~p.choose)));
~pages = Array.fill(~pageNum, {Array.fill(~lineNum, {Array.fill(~charNum ,"·")})});
~pagesOut = Array.fill(~pageNum, "");


~genpages = {
	~pages = Array.fill(~pageNum, {Array.fill(~lineNum, {Array.fill(~charNum ," ")})});
	~pageNum.do{arg p;
		~lineNum.do{arg l;
			var bdot = [~d, ~t].choose;
			var edot = [~d, ~t].choose;
			var bcox = [~c, bdot].scramble;
			var ecox = [~c, edot].scramble;
			var bs = bcox[0].choose + bcox[1].choose;
			var es = ecox[0].choose + ecox[1].choose;
			// [bs, es].postln;
			// [bs.size, es.size].postln;
			bs.do{arg c, i;
				~pages[p][l][i] = c;
			};			
			es.do{arg c, i;
				~pages[p][l][~charNum - es.size + i] = c;
			};
		}
	};
	~pagesOut = Array.fill(~pageNum, "");
	~pagesOut = ~pagesOut.collect{arg i, p;
		var op = "";
		// ~pages[p].size.postln;
		~pages[p].do{arg l;
			// l.size.postln;
			// op.size.postln;
			l.do{arg c; op = op ++ c};
			// op.last.postln;
		};		
		op;
	}
};


~genpages.();

// write out

~file = File.new("/home/david/src/towards/src/Texts/David.elm", "w");

~header = "module Texts.David exposing (texts)\n\n";
~header = ~header ++ "import Texts\n\n\n";
~header = ~header ++ "texts : List Texts.EntryWithIndex\n";
~header = ~header ++ "texts = ";

~file.write(~header);
~file.write("[ ");
~pagesOut.do{arg e, i;
	if (i==0, {~file.write("( ")}, {~file.write("   , ( ")});
	~file.write(i.asString);
	~file.write(", Texts.noNl \"");
	e.postln;
	~file.write(e);
	~file.write("\" )\n");
};

~file.write("    ]\n");
~file.close();


// snippets

("df" + "ff").size;

~genpages.();

~pages[49][1];

~pages.choose.choose;


~pagesOut[4];


[0, 1, 3, 4].scramble;


if(1, {"true".postln}, {"false".postln});

(~d[0] + ~t[2]).size;

~page = ~page.insert(1, "tesr1");

~page;

~tt = "asdasdasdasdasdasdasd";

~tt.insert(10, "0");

~tt;


~pages[0][1].size;

~pages[0][1];

~pages[0][20];

~pages = ~pages.()
