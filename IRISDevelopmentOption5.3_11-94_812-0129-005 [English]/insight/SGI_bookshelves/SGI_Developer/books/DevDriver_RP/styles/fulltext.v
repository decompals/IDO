<!-- Version $Revision: 1.1 $ of the fulltext stylesheet -->

<!ENTITY	atable.space-b4	CDATA	"12"	>
<!ENTITY	body.font-family	CDATA	"new century schoolbook"	>
<!ENTITY	body.font-size	CDATA	"12"	>
<!ENTITY	body.line-space	CDATA	"14"	>
<!ENTITY	bullet-color	CDATA	"grey30"	>
<!ENTITY	bullet.font-family	CDATA	"symbol"	>
<!ENTITY	bullet.font-size	CDATA	"14"	>
<!ENTITY	bullet.left-indent	CDATA	"+=18"	>
<!ENTITY	bullpara.space-b4	CDATA	"4"	>
<!ENTITY	chp.title.font-size	CDATA	"22"	>
<!ENTITY	chp.title.line-space	CDATA	"20"	>
<!ENTITY	chp.title.space-b4	CDATA	"5"	>
<!ENTITY	command-font	CDATA	"courier"	>
<!ENTITY	comment.left-indent	CDATA	"30"	>
<!ENTITY	comment.right-indent	CDATA	"5"	>
<!ENTITY	cross-link.color	CDATA	"#96000d"	>
<!ENTITY	default.foreground	CDATA	"grey20"	>
<!ENTITY	default.space-after	CDATA	"6"	>
<!ENTITY	default.space-b4	CDATA	"6"	>
<!ENTITY	doc.title.font-size	CDATA	"22"	>
<!ENTITY	doc.title.line-space	CDATA	"26"	>
<!ENTITY	files.left-indent	CDATA	"+=50"	>
<!ENTITY	gloss.first-indent	CDATA	"-20"	>
<!ENTITY	gloss.lbl.font-size	CDATA	"14"	>
<!ENTITY	gloss.lbl.line-space	CDATA	"18"	>
<!ENTITY	gloss.lbl.space-bef	CDATA	"20"	>
<!ENTITY	gloss.left-indent	CDATA	"50"	>
<!ENTITY	gloss.space-after	CDATA	"10"	>
<!ENTITY	hang.left-indent	CDATA	"+=50"	>
<!ENTITY	hot-link.color	CDATA	"#000078"	>
<!ENTITY	left-indent.1	CDATA	"12"	>
<!ENTITY	note.first-indent	CDATA	"-=36"	>
<!ENTITY	note.foreground	CDATA	"black"	>
<!ENTITY	note.space-aft	CDATA	"10"	>
<!ENTITY	note.space-before	CDATA	"10"	>
<!ENTITY	right-indent.1	CDATA	"10"	>
<!ENTITY	sec1.line-spacing	CDATA	"18"	>
<!ENTITY	sec1.space-before	CDATA	"30"	>
<!ENTITY	sec1.title.font-size	CDATA	"14"	>
<!ENTITY	sec2.space-before	CDATA	"20"	>
<!ENTITY	sec4.space-before	CDATA	"15"	>
<!ENTITY	space	CDATA	"  "	>
<!ENTITY	sqbul	CDATA	"n"	>
<!ENTITY	sqbul.font-size	CDATA	"8"	>
<!ENTITY	sqbullet.font-family	CDATA	"itc zapf dingbats"	>
<!ENTITY	table-font	CDATA	"helvetica"	>
<!ENTITY	table.space-before	CDATA	"2"	>
<!ENTITY	title	CDATA	"black"	>
<!ENTITY	title-font	CDATA	"helvetica"	>
<!ENTITY	title.left-indent	CDATA	"-6"	>
<!ENTITY	title.text.b4-lsp	CDATA	"+=2"	>
<!ENTITY	title.text.b4-size	CDATA	"14"	>
<!ENTITY	wp.font-family	CDATA	"screen"	>
<!ENTITY	wp.font-size	CDATA	"12"	>
<!ENTITY	wp.line-spacing	CDATA	"14"	>
<!ENTITY	wp.space-before	CDATA	"40"	>

<sheet >



<?INSTED COMMENT: GROUP #TAGS>

<group name="#TAGS">
	<font-family>	helvetica	</>
	<font-weight>	Medium	</>
	<font-size>	*	</>
	<foreground>	purple	</>
	<score>	Under	</>
</group>



<?INSTED COMMENT: GROUP note-caut-warn>

<group name="note-caut-warn">
	<first-indent>	&note.first-indent	</>
	<space-before>	&note.space-before	</>
	<space-after>	&note.space-aft	</>
	<break-before>	True	</>
	<text-before>Note:</>
</group>

<style name="BULLET,NOTE" group="note-caut-warn">
	<left-indent>	+=74	</>
</style>

<style name="CAUTION" group="note-caut-warn">
	<left-indent>	+=55	</>
	<first-indent>	-=55	</>
	<text-before>Caution:</>
</style>

<style name="LIST,CAUTION" group="note-caut-warn">
	<left-indent>	+=75	</>
	<first-indent>	-=55	</>
	<text-before>Caution:</>
</style>

<style name="LIST,NOTE" group="note-caut-warn">
	<left-indent>	+=54	</>
</style>

<style name="NOTE" group="note-caut-warn">
	<left-indent>	+=36	</>
</style>

<style name="SHORTCUT" group="note-caut-warn">
	<left-indent>	+=57	</>
	<first-indent>	-=57	</>
	<text-before>Shortcut:</>
</style>

<style name="WARNING" group="note-caut-warn">
	<left-indent>	+=57	</>
	<first-indent>	-=57	</>
	<text-before>Warning:</>
</style>



<?INSTED COMMENT: GROUP titles>

<group name="titles">
	<font-weight>	Bold	</>
	<foreground>	&title	</>
	<justification>	Left	</>
	<break-before>	True	</>
</group>

<style name="APPENDIX,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>if(isempty(attr(LBL,ancestor(APPENDIX))),'',Appendix attr(LBL,ancestor(APPENDIX)))</>
</style>

<style name="CHAPTER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>if(isempty(attr(LBL,ancestor(CHAPTER))),'',Chapter attr(LBL,ancestor(CHAPTER)))</>
</style>

<style name="GLOSSARY,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="INTRODUCTION,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	5	</>
	<hrule>	Before	</>
</style>

<style name="REFNAME" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
</style>

<style name="REFSECT1,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
</style>

<style name="REFSECT2,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec2.space-before	</>
</style>

<style name="REFSECT3,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec4.space-before	</>
</style>

<style name="SECTION1,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="SECTION2,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION2))),'',join(attr(LBL,ancestor(SECTION2)),'   '))</>
</style>

<style name="SECTION3,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec2.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION3))),'',join(attr(LBL,ancestor(SECTION3)),'   '))</>
</style>

<style name="SECTION4,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec4.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION4))),'',join(attr(LBL,ancestor(SECTION4)),'   '))</>
</style>

<style name="SPARES,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
</style>

<style name="SSB,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
</style>

<style name="SUBTITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
</style>

<style name="SUPPORT,TITLEPAGE,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="TITLEPAGE,TITLE" group="titles">
	<font-family>	helvetica	</>
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="WHITEPAPER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
	<hrule>	Before	</>
</style>



<?INSTED COMMENT: UNGROUPED STYLES FOLLOW>

<style name="#QUERY">
	<font-video>	Inverse	</>
	<foreground>	gray55	</>
</style>

<style name="#ROOT">
	<break-before>	Line	</>
</style>

<style name="#SDATA">
	<font-family>	attr(font)	</>
	<character-set>	attr(charset)	</>
	<text-before>char(attr(code))</>
</style>

<style name="#TAGS">
	<font-size>	10	</>
	<foreground>	purple	</>
	<line-spacing>	10	</>
</style>

<style name="APPENDIX">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="APPENDIX,TITLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
	<font-size>	&title.text.b4-size	</>
	<break-after>	True	</>
</style>

<style name="ARGUMENT">
	<font-slant>	Italics	</>
</style>

<style name="AUDIO">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	sound	</>
</style>

<style name="BLANKLINE">
	<space-before>	6	</>
	<break-before>	True	</>
</style>

<style name="BNF">
	<font-slant>	Italics	</>
</style>

<style name="BNFRULE">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
</style>

<style name="BNFTERM">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
</style>

<style name="BOLD">
	<font-weight>	Bold	</>
</style>

<style name="BULLET">
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>·</>
</style>

<style name="BULLET,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLET,CODE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BULLET,EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BULLET,PARAGRAPH">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETIND">
	<text-before>-</>
</style>

<style name="BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETIND,PARAGRAPH">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETLIST">
	<space-after>	0	</>
</style>

<style name="BULLETLIST,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETLIST,BULLET">
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>·</>
</style>

<style name="BULLETLIST,BULLET,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETLISTIND,BULLETIND">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>-</>
</style>

<style name="BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&body.font-family	</>
	<font-size>	&bullet.font-size	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETPARA*FIRST*FALSE">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&bullpara.space-b4	</>
	<break-before>	True	</>
</style>

<style name="BULLETPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="BULLETSQUAREIND">
	<left-indent>	&bullet.left-indent	</>
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>&sqbul</>
</style>

<style name="BULLETSQUAREIND,#TEXT-BEFORE">
	<font-family>	&sqbullet.font-family	</>
	<font-size>	&sqbul.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETSQUAREIND,CODE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BULLETSQUAREIND,NOTE">
	<left-indent>	+=54	</>
	<first-indent>	-=37	</>
	<space-before>	&note.space-before	</>
	<space-after>	&note.space-aft	</>
	<break-before>	True	</>
	<text-before>Note:</>
</style>

<style name="BULLETSQUAREIND,PARAGRAPH">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BUTTON">
	<font-slant>	Italics	</>
</style>

<style name="CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<left-indent>	&left-indent.1	</>
	<space-before>	8	</>
	<break-before>	True	</>
	<column>	False	</>
</style>

<style name="CAUTION,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="CELL">
	<left-indent>	if(eq(cnum(),1),12,int(mult(1.27,attr(LEFT))))	</>
	<width>	int(mult(1.27,attr(WIDTH)))	</>
	<column>	True	</>
</style>

<style name="CELL_PARAGRAPH">
	<space-before>	if(eq(cnum(),1),0,default.space-b4)	</>
	<break-before>	True	</>
</style>

<style name="CHAPTER">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="CHAPTER,TITLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
	<font-size>	&title.text.b4-size	</>
	<break-after>	True	</>
</style>

<style name="CODE">
	<font-family>	&command-font	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="CODE,SCREENDISPLAY">
	<font-family>	&command-font	</>
	<break-before>	Line	</>
</style>

<style name="CODE,USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
</style>

<style name="COMMAND">
	<font-slant>	Italics	</>
</style>

<style name="COMMENT">
	<font-slant>	Italics	</>
	<left-indent>	&comment.left-indent	</>
	<right-indent>	&comment.right-indent	</>
	<break-before>	True	</>
</style>

<style name="CONTRIBUTORS">
	<font-weight>	Bold	</>
	<space-before>	&chp.title.space-b4	</>
	<hide>	Children	</>
</style>

<style name="COORDINATE">
	<font-slant>	Italics	</>
</style>

<style name="COPYRIGHT">
	<font-weight>	Bold	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="COURIER">
	<font-family>	courier	</>
	<font-weight>	Medium	</>
	<font-slant>	Roman	</>
</style>

<style name="COURIERBOLD">
	<font-family>	courier	</>
	<font-weight>	Bold	</>
</style>

<style name="CREDITSPAGE">
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="CREDITSPAGE,PARAGRAPH">
	<hide>	Children	</>
	<break-after>	True	</>
</style>

<style name="DATE">
	<break-after>	True	</>
</style>

<style name="DEFINITION">
	<hide>	All	</>
</style>

<style name="DOCNUMBER">
	<space-before>	&chp.title.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="DOCTITLE">
	<font-slant>	Italics	</>
</style>

<style name="EMPHASIS">
	<font-slant>	Italics	</>
</style>

<style name="EXAMPLE">
	<font-family>	&command-font	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="EXAMPLE,FIGURE">
	<left-indent>	&left-indent.1	</>
	<space-before>	if(eq(file(env(HOME)/.figsInsight),FILE),0,6)	</>
	<space-after>	if(eq(file(env(HOME)/.figsInsight),FILE),0,6)	</>
</style>

<style name="EXAMPLE,VLINE">
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="EXTPROGRAM">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(PARMS)"	</>
	<icon-type>	extpgm	</>
</style>

<style name="EXTREF">
	<font-weight>	Bold	</>
	<foreground>	&cross-link.color	</>
	<script>	ebt-link book=attr(BOOK) tname="ID" tvalue=attr(IDREF)	</>
</style>

<style name="FIGURE">
	<left-indent>	&left-indent.1	</>
	<space-before>	if(eq(file(env(HOME)/.figsInsight),FILE),2,6)	</>
	<space-after>	if(eq(file(env(HOME)/.figsInsight),FILE),2,6)	</>
	<break-before>	True	</>
</style>

<style name="FIGURE,CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<first-indent>	-=30	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILE,lsibling(GRAPHIC)).hot),FILE),if(eq(file(env(HOME)/.figsInsight),FILE),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILE,lsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILE,lsibling(GRAPHIC))" title="content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE,lsibling(GRAPHIC)).hot),FILE),if(eq(file(env(HOME)/.figsInsight),FILE),empty,rasterhot),empty)	</>
</style>

<style name="FILENAME">
	<font-slant>	Italics	</>
</style>

<style name="FILESBODY">
	<left-indent>	&files.left-indent	</>
	<break-before>	Line	</>
	<column>	False	</>
</style>

<style name="FILESPAIR">
	<space-before>	&default.space-b4	</>
</style>

<style name="FTNOTE">
	<icon-position>	Right	</>
	<hide>	Children	</>
	<script>	ebt-reveal stylesheet=fulltext.v	</>
	<icon-type>	footnote	</>
</style>

<style name="FUNCTION">
	<font-slant>	Italics	</>
</style>

<style name="GENERALINFO">
	<space-before>	&default.space-b4	</>
</style>

<style name="GLOSSARY">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYDEF">
	<space-after>	&gloss.space-after	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYENTRY">
	<font-weight>	Bold	</>
	<first-indent>	&gloss.first-indent	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="GLOSSARYITEM">
	<score>	Under	</>
	<script>	sgi-glossary window=new book=glossary root="'parent(query(<GLOSSARYENTRY> containing 'content(me())'))'"	</>
</style>

<style name="GLOSSARYTERM">
	<left-indent>	&gloss.left-indent	</>
	<break-before>	True	</>
</style>

<style name="GRAPHIC">
	<select>	if(eq(file(env(HOME)/.figsInsight),FILE),GRAPHIC_OUT,GRAPHIC_IN)	</>
</style>

<style name="GRAPHIC_IN">
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<inline>	if(contains(attr(FILE),.cgm),vector,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILE)"	</>
</style>

<style name="GRAPHIC_OUT">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE).hot),FILE),rasterhot,raster)	</>
</style>

<style name="HANGBODY">
	<break-before>	False	</>
	<break-after>	False	</>
	<select>	if(lt(sub(length(content(lsibling(HANGITEM))),7),5),sameline,nextline)	</>
</style>

<style name="HANGBODY,BULLETLISTIND,BULLETIND">
	<left-indent>	&hang.left-indent	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>·</>
</style>

<style name="HANGBODY,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="HANGBODY,CODE">
	<font-family>	&command-font	</>
	<left-indent>	103	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="HANGBODY,ORDEREDLISTIND,LISTIND">
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="HANGBODY,PARAGRAPH">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
</style>

<style name="HANGBODYIND">
	<select>	if(le(sub(length(content(lsibling(HANGITEM))),8),5),sameline,nextline)	</>
</style>

<style name="HANGBODYIND,PARAGRAPH">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
</style>

<style name="HANGITEM">
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
</style>

<style name="HANGLISTIND">
	<left-indent>	+=50	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="HANGPAIR">
	<space-before>	5	</>
</style>

<style name="HANGPAIR,PARAGRAPH">
	<break-before>	True	</>
</style>

<style name="HANGPAIRIND">
	<space-before>	5	</>
	<break-after>	True	</>
</style>

<style name="HARDWARELABEL">
	<font-weight>	Bold	</>
</style>

<style name="HEADER">
	<space-before>	&default.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
</style>

<style name="HELVETICABOLD">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<font-slant>	Roman	</>
</style>

<style name="IDX">
	<space-before>	6	</>
	<space-after>	6	</>
</style>

<style name="IMAGE">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	vector	</>
</style>

<style name="INDEXTERM">
	<hide>	Text	</>
</style>

<style name="INLINE">
	<select>	if(eq(file(env(HOME)/.figsInsight),FILE),INLINE_OUT,INLINE_IN)	</>
</style>

<style name="INLINE,CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<icon-position>	if(eq(file(env(HOME)/.figsInsight),FILE),Off,Right)	</>
	<break-before>	True	</>
	<script>	sgi-custom type="attr(TYPE,lsibling(INLINE))" parm1="attr(PARM1,lsibling(INLINE))" parm2="attr(PARM2,lsibling(INLINE))" parm3="attr(PARM3,lsibling(INLINE))" parm4="attr(PARM4,lsibling(INLINE))" parm5="attr(PARM5,lsibling(INLINE))" parm6="attr(PARM6,lsibling(INLINE))" parm7="attr(PARM7,lsibling(INLINE))"	</>
	<icon-type>	empty	</>
</style>

<style name="INLINEOBJECT">
	<left-indent>	&left-indent.1	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="INLINE_IN">
	<space-before>	&default.space-b4	</>
	<inline>	custom type="attr(TYPE)" parm1="attr(PARM1)" parm2="attr(PARM2)" parm3="attr(PARM3)" parm4="attr(PARM4)" parm5="attr(PARM5)" parm6="attr(PARM6)" parm7="attr(PARM7)"	</>
</style>

<style name="INLINE_OUT">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	sgi-custom type="attr(TYPE)" parm1="attr(PARM1)" parm2="attr(PARM2)" parm3="attr(PARM3)" parm4="attr(PARM4)" parm5="attr(PARM5)" parm6="attr(PARM6)" parm7="attr(PARM7)"	</>
	<icon-type>	media	</>
</style>

<style name="INTRODUCTION">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="IRIXCOMMAND">
	<font-family>	&command-font	</>
</style>

<style name="ITALICS">
	<font-slant>	Italics	</>
</style>

<style name="KEYWORDS">
	<break-after>	True	</>
</style>

<style name="LABEL">
	<font-weight>	Bold	</>
	<font-size>	&gloss.lbl.font-size	</>
	<line-spacing>	&gloss.lbl.line-space	</>
	<space-before>	&gloss.lbl.space-bef	</>
</style>

<style name="LIST,BULLETLISTIND,BULLETIND">
	<left-indent>	&bullet.left-indent	</>
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>·</>
</style>

<style name="LIST,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="LIST,CODE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="LIST,EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="LIST,PARAGRAPH">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTIND,PARAGRAPH">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="MANUAL">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<foreground>	&default.foreground	</>
	<left-indent>	&left-indent.1	</>
	<right-indent>	&right-indent.1	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	False	</>
	<break-after>	True	</>
	<column>	False	</>
</style>

<style name="MANUAL,FRONTMATTER">
	<foreground>	&hot-link.color	</>
	<space-before>	10	</>
	<icon-position>	Right	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<script>	ebt-link root=me() window=new stylesheet=frontmatter	</>
	<icon-type>	copyrt	</>
</style>

<style name="MARGINTEXT">
	<icon-position>	Left	</>
	<hide>	Children	</>
	<script>	ebt-reveal stylesheet=fulltext.v title="Margin Text" width="100"	</>
	<icon-type>	margin	</>
</style>

<style name="MENUCHOICE">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="MENUNAME">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="NAMES">
	<font-weight>	Medium	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="NEWLINE">
	<break-before>	True	</>
</style>

<style name="NEWTERM">
	<font-slant>	Italics	</>
</style>

<style name="NEXTLINE">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
</style>

<style name="NOFILL">
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="NONPRKEYS">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
</style>

<style name="NORMAL_PARAGRAPH">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
</style>

<style name="NOTE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="ORDEREDLIST">
	<space-after>	5	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="ORDEREDLIST,LIST">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="ORDEREDLISTIND,LISTIND">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="PARAGRAPH">
	<select>	if(eq(tag(ancestor()),CELL),CELL_PARAGRAPH,NORMAL_PARAGRAPH)	</>
</style>

<style name="PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
	<font-size>	&body.font-size	</>
</style>

<style name="PROGRAMNAME">
	<font-slant>	Italics	</>
</style>

<style name="QANDA">
	<space-before>	&default.space-b4	</>
</style>

<style name="REFCLASS">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<line-spacing>	12	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="REFENTRY">
	<break-before>	True	</>
</style>

<style name="REFNAMEDIV">
	<break-before>	True	</>
</style>

<style name="REFPURPOSE">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<space-before>	10	</>
	<break-before>	True	</>
</style>

<style name="REFSECT1">
	<break-before>	True	</>
</style>

<style name="REFSECT2">
	<break-before>	True	</>
</style>

<style name="REFSECT3">
	<break-before>	True	</>
</style>

<style name="ROW">
	<left-indent>	&left-indent.1	</>
	<!--	<space-before>	&table.space-before	</> -->
	<break-before>	True	</>
</style>

<style name="SAMELINE">
	<left-indent>	&bullet.left-indent	</>
</style>

<style name="SCREENDISPLAY">
	<font-family>	&command-font	</>
	<break-before>	True	</>
</style>

<style name="SECTION1">
	<break-before>	True	</>
</style>

<style name="SECTION2">
	<break-before>	True	</>
</style>

<style name="SECTION3">
	<break-before>	True	</>
</style>

<style name="SGIINDEX">
	<hide>	All	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before>Index</>
</style>

<style name="SGIINDEX,#TEXT-BEFORE">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
</style>

<style name="SHORTCUT,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="SP">
	<break-before>	False	</>
	<break-after>	False	</>
	<text-before>   </>
</style>

<style name="SPARES">
	<break-before>	True	</>
</style>

<style name="SPARESBODY">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="SPARESBODY,EXAMPLE">
	<font-family>	&wp.font-family	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="SSB">
	<break-before>	True	</>
</style>

<style name="SSBBODY">
	<select>	SPARESBODY	</>
</style>

<style name="SSBBODY,EXAMPLE">
	<font-family>	&wp.font-family	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="SSLIST">
	<break-before>	False	</>
</style>

<style name="SSLIST,LISTIND">
	<left-indent>	+=40	</>
	<first-indent>	-20	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>format(cnum(),letter).</>
</style>

<style name="SSLIST,LISTIND,PARAGRAPH">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="SUBSCRIPT">
	<vertical-offset>	-3	</>
</style>

<style name="SUPERSCRIPT">
	<vertical-offset>	4	</>
</style>

<style name="SUPPORT">
	<font-family>	&wp.font-family	</>
	<font-size>	&wp.font-size	</>
	<foreground>	&default.foreground	</>
	<left-indent>	&left-indent.1	</>
	<right-indent>	&right-indent.1	</>
	<line-spacing>	&wp.line-spacing	</>
	<break-before>	False	</>
	<break-after>	True	</>
	<column>	False	</>
</style>

<style name="SYM">
	<font-family>	symbol	</>
	<font-weight>	Medium	</>
	<character-set>	symbol	</>
</style>

<style name="SYNOPSIS">
	<font-family>	courier	</>
	<justification>	Left	</>
	<break-before>	True	</>
	<text-before>Synopsis      </>
</style>

<style name="SYNOPSIS,#TEXT-BEFORE">
	<font-family>	&title-font	</>
	<font-weight>	Bold	</>
	<font-size>	&sec1.title.font-size	</>
	<space-before>	15	</>
	<space-after>	5	</>
	<break-before>	True	</>
</style>

<style name="SYNOPSIS,NEWLINE,ARGUMENT">
	<font-slant>	Italics	</>
</style>

<style name="TABLE">
	<font-family>	helvetica	</>
	<font-size>	&body.font-size	</>
	<space-after>	&default.space-after	</>
	<justification>	Left	</>
</style>

<style name="TABLE,#TEXT-BEFORE">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
</style>

<style name="TABLE.F,TABLEROW,TABLECELL">
	<left-indent>	div(tableinfo(arbor,left-indent),2)	</>
	<width>	sub(div(tableinfo(arbor,width),2),15)	</>
	<justification>	tableinfo(arbor,justification)	</>
	<column>	True	</>
</style>

<style name="TABLECAPTION">
	<font-weight>	Bold	</>
	<left-indent>	12	</>
	<space-before>	5	</>
	<space-after>	10	</>
	<justification>	Left	</>
	<break-before>	True	</>
</style>

<style name="TABLECAPTION,#TEXT-BEFORE">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
</style>

<style name="TABLECELL">
	<left-indent>	+=div(tableinfo(arbor,left-indent),1.8)	</>
	<width>	sub(div(tableinfo(arbor,width),1),1)	</>
	<justification>	tableinfo(arbor,justification)	</>
	<column>	True	</>
</style>

<style name="TABLECELL,PARAGRAPH">
	<line-spacing>	-=3	</>
	<space-before>	if(eq(cnum(),1),0,^\default.space-b4)	</>
	<break-before>	True	</>
</style>

<style name="TABLEFOOTNOTE">
	<space-before>	10	</>
	<break-before>	True	</>
</style>

<style name="TABLEHEADING">
	<font-weight>	Bold	</>
	<score>	Under	</>
	<break-before>	True	</>
</style>

<style name="TABLEROW">
	<left-indent>	&left-indent.1	</>
	<!--    <space-before>  &table.space-before     </> -->
	<break-before>	True	</>
	<select>	TABLEROW,TABLEROW.attr(hdr),TABLEROW	</>
</style>

<style name="TABLEROW.1">
	<font-weight>	Bold	</>
	<font-slant>	Roman	</>
	<score>	Under	</>
</style>

<style name="TABLE_F">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<foreground>	black	</>
	<left-indent>	13	</>
	<width>	220	</>
	<space-before>	&table.space-before	</>
	<space-after>	10	</>
	<justification>	Left	</>
	<icon-position>	Right	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<script>	ebt-link root=me() window=new stylesheet=tables.v hscroll=yes  width=700 height=500	</>
	<icon-type>	table	</>
	<text-before>content(typechild('tablecaption'))         </>
</style>

<style name="TABLE_T">
	<font-family>	helvetica	</>
	<space-before>	&table.space-before	</>
	<space-after>	8	</>
	<break-before>	True	</>
	<text-before>content(typechild('tablecaption'))         </>
</style>

<style name="TECHNICAL">
	<font-weight>	Bold	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="VARIABLE">
	<font-slant>	Italics	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="VBLOCK">
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="VIDEO">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	video	</>
</style>

<style name="WARNING,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="XREF">
	<font-weight>	Bold	</>
	<foreground>	&hot-link.color	</>
	<select>	XREF,XREF_attr(TYPE)	</>
</style>

<style name="XREF,COURIERBOLD">
	<font-family>	courier	</>
	<font-weight>	Bold	</>
	<foreground>	&hot-link.color	</>
	<script>	ebt-link target=idmatch(ID,attr(IDREF,ancestor(XREF)))	</>
</style>

<style name="XREF,ITALICS">
	<font-family>	new century schoolbook	</>
	<font-weight>	Medium	</>
	<font-slant>	Italics	</>
	<foreground>	&hot-link.color	</>
	<script>	ebt-link target=idmatch(ID,attr(IDREF,ancestor(XREF)))	</>
</style>

<style name="XREF_">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="XREF_GRAPHIC">
	<script>	ebt-if(contains(attr(FILE,lsibling(GRAPHIC,ancestor(CAPTION,idmatch(ID,attr(IDREF))))),.cgm),vector,raster) filename="attr(FILE,lsibling(GRAPHIC,ancestor(CAPTION,idmatch(ID,attr(IDREF)))))" title="join(change(content(ancestor(CAPTION,idmatch(ID,attr(IDREF)))),\",\'\',TRUE))"	</>
</style>

<style name="XREF_REFNAME">
	<script>	ebt-link target=idmatch(ID,@(IDREF))	</>
</style>

<style name="XREF_TABLE">
	<script>	ebt-link target=ancestor(ancestor(idmatch(ID,attr(IDREF))))	</>
</style>

<style name="XREF_TITLE">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="ZAPF">
	<font-family>	itc zapf dingbats	</>
	<character-set>	symbol	</>
</style>



</sheet>
