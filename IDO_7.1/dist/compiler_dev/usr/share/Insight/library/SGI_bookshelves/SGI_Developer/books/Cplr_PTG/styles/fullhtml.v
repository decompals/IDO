<!-- Version $Revision: 1.15 $ of the fulltext stylesheet -->

<!ENTITY	app.text-b4	CDATA	"<HR><H1>if(isempty(attr(LBL,ancestor(APPENDIX))),'',switch(attr(LANG,ancestor(SGIDOC)),de,Anhang ,fr,Appendice ,ja_JP.EUC,ÉÕÏ¿ ,DEFAULT,Appendix ) attr(LBL,ancestor(APPENDIX))<BR>) "	>
<!ENTITY	body.font-family	CDATA	"new century schoolbook"	>
<!ENTITY	body.font-size	CDATA	"12"	>
<!ENTITY	body.line-space	CDATA	"17"	>
<!ENTITY	bullet-color	CDATA	"grey30"	>
<!ENTITY	bullet.font-family	CDATA	"symbol"	>
<!ENTITY	bullet.font-size	CDATA	"14"	>
<!ENTITY	bullet.left-indent	CDATA	"+=18"	>
<!ENTITY	bullpara.space-b4	CDATA	"6"	>
<!ENTITY	chp.text-b4	CDATA	"<HR><H1>if(isempty(attr(LBL,ancestor(CHAPTER))),'',switch(attr(LANG,ancestor(SGIDOC)),de,Kapitel ,fr,Chapitre ,ja_JP.EUC,Âè ,DEFAULT,Chapter ) attr(LBL,ancestor(CHAPTER)) switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC, ¾Ï,DEFAULT,)<BR>) "	>
<!ENTITY	chp.title.font-size	CDATA	"22"	>
<!ENTITY	chp.title.line-space	CDATA	"28"	>
<!ENTITY	chp.title.space-b4	CDATA	"36"	>
<!ENTITY	command-font	CDATA	"courier"	>
<!ENTITY	comment.left-indent	CDATA	"30"	>
<!ENTITY	comment.right-indent	CDATA	"5"	>
<!ENTITY	cross-link.color	CDATA	"#96000d"	>
<!ENTITY	default.foreground	CDATA	"grey20"	>
<!ENTITY	default.ind.space-b4	CDATA	"6"	>
<!ENTITY	default.left-indent	CDATA	"2"	>
<!ENTITY	default.space-after	CDATA	"6"	>
<!ENTITY	default.space-b4	CDATA	"12"	>
<!ENTITY	doc.title.font-size	CDATA	"24"	>
<!ENTITY	doc.title.line-space	CDATA	"30"	>
<!ENTITY	gloss.first-indent	CDATA	"32"	>
<!ENTITY	gloss.lbl.font-size	CDATA	"14"	>
<!ENTITY	gloss.lbl.line-space	CDATA	"20"	>
<!ENTITY	gloss.lbl.space-bef	CDATA	"20"	>
<!ENTITY	gloss.left-indent	CDATA	"18"	>
<!ENTITY	gloss.space-after	CDATA	"0"	>
<!ENTITY	hang.left-indent	CDATA	"+=100"	>
<!ENTITY	hot-link.color	CDATA	"#000078"	>
<!ENTITY	index.text-b4	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),de,Index,fr,Index,ja_JP.EUC,º÷°ú,DEFAULT,Index)"	>
<!ENTITY	left-indent.1	CDATA	"48"	>
<!ENTITY	list.left-indent	CDATA	"if(or(tag(ancestor(EXPLANATION)),tag(ancestor(HANGBODY))),+=0,+=18)"	>
<!ENTITY	note.foreground	CDATA	"black"	>
<!ENTITY	right-indent.1	CDATA	"10"	>
<!ENTITY	sec1.line-spacing	CDATA	"22"	>
<!ENTITY	sec1.space-before	CDATA	"14"	>
<!ENTITY	sec1.title.font-size	CDATA	"22"	>
<!ENTITY	sec2.space-before	CDATA	"14"	>
<!ENTITY	sec3.space-before	CDATA	"12"	>
<!ENTITY	sec4.space-before	CDATA	"15"	>
<!ENTITY	space	CDATA	"  "	>
<!ENTITY	sqbul	CDATA	"n"	>
<!ENTITY	sqbul.font-size	CDATA	"8"	>
<!ENTITY	sqbullet.font-family	CDATA	"itc zapf dingbats"	>
<!ENTITY	table-font	CDATA	"helvetica"	>
<!ENTITY	title	CDATA	"black"	>
<!ENTITY	title-font	CDATA	"new century schoolbook"	>
<!ENTITY	title.left-indent	CDATA	"-6"	>
<!ENTITY	title.text.b4-lsp	CDATA	"0"	>
<!ENTITY	title.text.b4-size	CDATA	"13"	>
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



<?INSTED COMMENT: GROUP address>

<!-- Template for new revelatory Stylesheets

  Copyright 1994, Electronic Book Technologies.  All rights reserved.
-->
<group name="address">
	<font-slant>	Italics	</>
	<break-before>	Line	</>
	<text-before><address></>
	<text-after>join('<','/address>')</>
</group>



<?INSTED COMMENT: GROUP b>

<group name="b">
	<font-weight>	Bold	</>
	<text-before><b></>
	<text-after>join('<','/b>')</>
</group>

<style name="ARGUMENT" group="b">
	<font-family>	&body.font-family	</>
	<font-weight>	Bold	</>
</style>

<style name="BOLD" group="b">
	<font-weight>	Bold	</>
</style>

<style name="CONTRIBUTORS" group="b">
	<font-weight>	Bold	</>
	<space-before>	&chp.title.space-b4	</>
	<text-before><P><B></>
</style>

<style name="COPYRIGHT" group="b">
	<font-weight>	Bold	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before><P><B></>
	<text-after>join('<','/B>')</>
</style>

<style name="FUNCTION" group="b">
	<font-family>	&body.font-family	</>
	<font-weight>	Bold	</>
</style>

<style name="HARDWARELABEL" group="b">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
</style>

<style name="KEYWORD" group="b">
	<font-family>	&body.font-family	</>
	<font-weight>	Bold	</>
</style>

<style name="LABEL" group="b">
	<font-weight>	Bold	</>
	<font-size>	&gloss.lbl.font-size	</>
	<line-spacing>	&gloss.lbl.line-space	</>
	<space-before>	&gloss.lbl.space-bef	</>
</style>

<style name="NONPRKEYS" group="b">
</style>

<style name="TECHNICAL" group="b">
	<font-weight>	Bold	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before><P><B></>
	<text-after>join('<','/B>')</>
</style>



<?INSTED COMMENT: GROUP blockquote>

<group name="blockquote">
	<left-indent>	+=10	</>
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><blockquote></>
	<text-after>join('<','/blockquote>')</>
</group>



<?INSTED COMMENT: GROUP br>

<group name="br">
	<break-before>	Line	</>
	<text-before><br></>
</group>

<style name="NEWLINE" group="br">
	<break-before>	True	</>
	<text-before>if(contains(tag(ancestor(ancestor())),HANGBODY),,<br>)</>
</style>



<?INSTED COMMENT: GROUP cells>

<group name="cells">
	<left-indent>	if(eq(cnum(),1),48,int(add(48,mult(1.45,attr(LEFT)))))	</>
	<width>	int(mult(1.45,attr(WIDTH)))	</>
	<text-before>if(ancestor(TABLEHEADING),<TH>,<TD>))</>
	<text-after>if(ancestor(TABLEHEADING),join('<','/TH>'),join('<','/TD>'))</>
	<column>	True	</>
</group>

<style name="BULLET,TABLE,TABLEBODY,ROW,CELL" group="cells">
	<left-indent>	if(lsibling(CAPTION,ancestor(TABLEBODY)), if(eq(cnum(),1),48,int(add(48,mult(1.45,attr(LEFT))))), if(eq(cnum(),1),66,int(add(66,mult(1.4,attr(LEFT))))))	</>
	<width>	int(mult(1.4,attr(WIDTH)))	</>
</style>

<style name="CELL" group="cells">
</style>

<style name="HANGBODY,TABLE,TABLEBODY,ROW,CELL" group="cells">
	<left-indent>	if(lsibling(CAPTION,ancestor(TABLEBODY)), if(eq(cnum(),1),48,int(add(48,mult(1.45,attr(LEFT))))), if(eq(cnum(),1),148,int(add(148,mult(1.4,attr(LEFT))))))	</>
	<width>	int(mult(1.4,attr(WIDTH)))	</>
</style>

<style name="LIST,TABLE,TABLEBODY,ROW,CELL" group="cells">
	<left-indent>	if(lsibling(CAPTION,ancestor(TABLEBODY)), if(eq(cnum(),1),48,int(add(48,mult(1.45,attr(LEFT))))), if(eq(cnum(),1),66,int(add(66,mult(1.4,attr(LEFT))))))	</>
	<width>	int(mult(1.4,attr(WIDTH)))	</>
</style>



<?INSTED COMMENT: GROUP cite>

<group name="cite">
	<font-slant>	Italics	</>
	<text-before><cite></>
	<text-after>join('<','/cite>')</>
</group>



<?INSTED COMMENT: GROUP code>

<group name="code">
	<font-family>	courier	</>
	<text-before><code></>
	<text-after>join('<','/code>')</>
</group>



<?INSTED COMMENT: GROUP dd>

<group name="dd">
	<left-indent>	+=55	</>
	<text-before><dd></>
</group>

<style name="BNFRULE" group="dd">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<text-before><dd><i></>
	<text-after>join('<','/i><','/dd>')</>
</style>

<style name="HANGBODY,PARAGRAPH" group="dd">
	<line-spacing>	&body.line-space	</>
	<break-before>	if(lsibling(PARAGRAPH),True,False)	</>
	<break-after>	True	</>
	<text-before><dd><br></>
</style>

<style name="HANGBODYIND,PARAGRAPH" group="dd">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	False	</>
	<break-after>	True	</>
	<text-before><dd><br></>
</style>



<?INSTED COMMENT: GROUP dir>

<group name="dir">
	<left-indent>	+=10	</>
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><dir></>
	<text-after>join('<','/dir>')</>
</group>



<?INSTED COMMENT: GROUP dl>

<group name="dl">
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><dl></>
	<text-after>join('<','/dl>')</>
</group>

<style name="BNF" group="dl">
	<font-slant>	Italics	</>
</style>

<style name="HANGLIST" group="dl">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="HANGLISTIND" group="dl">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before><br><br><dl></>
</style>



<?INSTED COMMENT: GROUP dt>

<group name="dt">
	<width>	50	</>
	<break-before>	Line	</>
	<text-before><dt></>
</group>

<style name="BNFTERM" group="dt">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
	<text-before><dt><i></>
	<text-after>join('<','/i><','/dt>')</>
</style>

<style name="HANGITEM" group="dt">
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before><dt>if(isfirst(ancestor(HANGPAIR)),, if(typechild(typechild(HANGBODY,lsibling(ancestor(HANGPAIR)))), if(or(contains(tag(nchild(-1,typechild(HANGBODY,lsibling(ancestor(HANGPAIR))))),EXAMPLE),contains(tag(nchild(-1,typechild(HANGBODY,lsibling(ancestor(HANGPAIR))))),CODE)),,<br>),<br><br>))</>
</style>



<?INSTED COMMENT: GROUP em>

<group name="em">
	<font-slant>	Italics	</>
	<text-before><em></>
	<text-after>join('<','/em>')</>
</group>

<style name="EMPHASIS" group="em">
	<font-slant>	Italics	</>
	<hide>	if(ancestor(TABLE,ancestor(CAPTION)),ALL,)	</>
</style>



<?INSTED COMMENT: GROUP h1>

<group name="h1">
	<font-weight>	Bold	</>
	<font-size>	24	</>
	<line-spacing>	29	</>
	<break-before>	Line	</>
	<text-before><hr><h1></>
	<text-after>join('<','/h1>')</>
</group>

<style name="APPENDIX,TITLE" group="h1">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>&app.text-b4</>
</style>

<style name="CHAPTER,TITLE" group="h1">
	<font-size>	&chp.title.font-size	</>
	<left-indent>	48	</>
	<first-indent>	0	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>&chp.text-b4</>
</style>

<style name="GLOSSARY,TITLE" group="h1">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="HELPTOPIC,TITLE" group="h1">
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<hide>	All	</>
	<break-before>	if(ancestor(REFERENCE),true,false)	</>
</style>

<style name="INTRODUCTION,TITLE" group="h1">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="PART,TITLE" group="h1">
	<font-size>	22	</>
	<space-before>	6	</>
	<hrule>	Before	</>
	<text-before><H1>PART switch(attr(LBL,ancestor(PART)),I,ONE,II,TWO,III,THREE,IV,FOUR,V,FIVE,VI,SIX,VII,SEVEN,VIII,EIGHT,IX,NINE,DEFAULT,TEN)<BR></>
</style>

<style name="REFERENCE,TITLE" group="h1">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hide>	if(ancestor(APPENDIX),Text,none)	</>
	<hrule>	if(ancestor(APPENDIX),None,Before)	</>
</style>



<?INSTED COMMENT: GROUP h2>

<group name="h2">
	<font-weight>	Bold	</>
	<font-size>	18	</>
	<line-spacing>	22	</>
	<break-before>	Line	</>
	<text-before><HR><h2></>
	<text-after>join('<','/h2>')</>
</group>

<style name="REFSYNOPSISDIV,TITLE" group="h2">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<font-size>	18	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before><h2>if(isempty(attr(LBL,ancestor(SECTION2))),'',join(attr(LBL,ancestor(SECTION2)),'   '))</>
</style>

<style name="SECTION1,TITLE" group="h2">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before><HR><h2>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="SPARES,TITLE" group="h2">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
</style>

<style name="SSB,TITLE" group="h2">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
</style>

<style name="SUBTITLE" group="h2">
	<font-size>	&chp.title.font-size	</>
	<left-indent>	48	</>
	<line-spacing>	&chp.title.line-space	</>
</style>



<?INSTED COMMENT: GROUP h3>

<group name="h3">
	<font-weight>	Bold	</>
	<break-before>	Line	</>
	<text-before><h3></>
	<text-after>join('<','/h3>')</>
</group>

<style name="SECTION2,TITLE" group="h3">
	<font-size>	18	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before><h3>if(isempty(attr(LBL,ancestor(SECTION2))),'',join(attr(LBL,ancestor(SECTION2)),'   '))</>
</style>



<?INSTED COMMENT: GROUP h4>

<group name="h4">
	<font-weight>	Bold	</>
	<font-size>	12	</>
	<line-spacing>	14	</>
	<break-before>	Line	</>
	<text-before><h4></>
	<text-after>join('<','/h4>')</>
</group>

<style name="SECTION3,TITLE" group="h4">
	<font-size>	14	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec3.space-before	</>
	<text-before><h4>if(isempty(attr(LBL,ancestor(SECTION3))),'',join(attr(LBL,ancestor(SECTION3)),'   '))</>
</style>



<?INSTED COMMENT: GROUP h5>

<group name="h5">
	<font-weight>	Bold	</>
	<font-size>	10	</>
	<line-spacing>	12	</>
	<break-before>	Line	</>
	<text-before><h5></>
	<text-after>join('<','/h5>')</>
</group>

<style name="SECTION4,TITLE" group="h5">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec4.space-before	</>
	<text-before><h5>if(isempty(attr(LBL,ancestor(SECTION4))),'',join(attr(LBL,ancestor(SECTION4)),'   '))</>
</style>



<?INSTED COMMENT: GROUP h6>

<group name="h6">
	<font-weight>	Bold	</>
	<font-size>	8	</>
	<line-spacing>	10	</>
	<break-before>	Line	</>
	<text-before><h6></>
	<text-after>join('<','/h6>')</>
</group>



<?INSTED COMMENT: GROUP hr>

<group name="hr">
	<hrule>	Before	</>
	<text-before><hr></>
</group>



<?INSTED COMMENT: GROUP i>

<group name="i">
	<font-slant>	Italics	</>
	<text-before><i></>
	<text-after>join('<','/i>')</>
</group>

<style name="BUTTON" group="i">
	<font-slant>	Italics	</>
</style>

<style name="COMMAND" group="i">
	<font-slant>	Italics	</>
</style>

<style name="COMMENT" group="i">
	<font-slant>	Italics	</>
	<left-indent>	&comment.left-indent	</>
	<right-indent>	&comment.right-indent	</>
	<break-before>	True	</>
</style>

<style name="COORDINATE" group="i">
	<font-slant>	Italics	</>
</style>

<style name="DOCTITLE" group="i">
	<font-slant>	Italics	</>
</style>

<style name="FILENAME" group="i">
	<font-slant>	Italics	</>
</style>

<style name="ITALICS" group="i">
	<font-slant>	Italics	</>
</style>

<style name="NEWTERM" group="i">
	<font-slant>	Italics	</>
</style>



<?INSTED COMMENT: GROUP kbd>

<group name="kbd">
	<font-family>	courier	</>
	<text-before><kbd></>
	<text-after>join('<','/kbd>')</>
</group>



<?INSTED COMMENT: GROUP li>

<group name="li">
	<left-indent>	+=10	</>
	<first-indent>	-10	</>
	<break-before>	Line	</>
	<text-before><li></>
	<text-after>join('<','/li>')</>
</group>



<?INSTED COMMENT: GROUP menu>

<group name="menu">
	<left-indent>	+=10	</>
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><menu></>
	<text-after>join('<','/menu>')</>
</group>



<?INSTED COMMENT: GROUP note-caut-warn>

<group name="note-caut-warn">
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'BULLETSQUAREIND',+=18,'LIST',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<text-before><P></>
</group>

<style name="CAUTION" group="note-caut-warn">
</style>

<style name="HINT" group="note-caut-warn">
	<break-before>	Line	</>
</style>

<style name="NOTE" group="note-caut-warn">
</style>

<style name="SHORTCUT" group="note-caut-warn">
</style>

<style name="TIP" group="note-caut-warn">
	<break-before>	Line	</>
</style>

<style name="WARNING" group="note-caut-warn">
	<font-weight>	Bold	</>
	<text-before><P><B></>
	<text-after>join('<','/B>')</>
</style>



<?INSTED COMMENT: GROUP ol>

<group name="ol">
	<left-indent>	+=10	</>
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><ol></>
	<text-after>join('<','/ol>')</>
</group>

<style name="ORDEREDLIST" group="ol">
	<space-after>	5	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="ORDEREDLISTIND" group="ol">
	<left-indent>	&list.left-indent	</>
</style>



<?INSTED COMMENT: GROUP p>

<group name="p">
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><p></>
</group>

<style name="GLOSSARYDEF" group="p">
	<left-indent>	&left-indent.1	</>
	<space-after>	&gloss.space-after	</>
	<break-before>	True	</>
	<text-before><dd><p></>
	<text-after>join('<','/dd>')</>
</style>

<style name="INDEXTERM" group="p">
</style>

<style name="LISTPARA*FIRST*FALSE" group="p">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
</style>

<style name="NORMAL_PARAGRAPH" group="p">
	<space-before>	if(isfirst(),6,switch(tag(ancestor(*,me(),1)), PART,12,INTRODUCTION,12,CHAPTER,12,APPENDIX,12,SECTION1,12,SECTION2,12,SECTION3,12,REFSECT1,12,REFSECT2,12,REFSECT3,12,REFNAME,12,DEFAULT,6))	</>
	<break-before>	True	</>
</style>



<?INSTED COMMENT: GROUP pre>

<group name="pre">
	<font-family>	courier	</>
	<space-before>	14	</>
	<justification>	Verbatim	</>
	<break-before>	Line	</>
	<text-before><pre></>
	<text-after>join('<','/pre>')</>
</group>



<?INSTED COMMENT: GROUP samp>

<group name="samp">
	<font-family>	courier	</>
	<text-before><samp></>
	<text-after>join('<','/samp>')</>
</group>



<?INSTED COMMENT: GROUP strong>

<group name="strong">
	<font-weight>	Bold	</>
	<text-before><strong></>
	<text-after>join('<','/strong>')</>
</group>



<?INSTED COMMENT: GROUP titles>

<group name="titles">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<foreground>	&title	</>
	<justification>	Left	</>
	<break-before>	True	</>
</group>

<style name="REFMETA" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="REFNAMEDIV,#TEXT-BEFORE" group="titles">
	<font-size>	18	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="SUPPORT,TITLEPAGE,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
	<text-before><head></>
	<text-after>join('<','/head><body><hr>')</>
</style>

<style name="TITLE" group="titles">
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<break-before>	if(ancestor(REFERENCE),true,false)	</>
</style>

<style name="TITLEPAGE,TITLE" group="titles">
	<font-size>	&doc.title.font-size	</>
	<left-indent>	48	</>
	<line-spacing>	&doc.title.line-space	</>
	<break-before>	false	</>
	<text-before><h1></>
	<text-after>join('<','/h1>')</>
</style>

<style name="WHITEPAPER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
	<text-before><head></>
	<text-after>join('<','/head><body><hr>')</>
</style>



<?INSTED COMMENT: GROUP tt>

<group name="tt">
	<font-family>	courier	</>
	<text-before><tt></>
	<text-after>join('<','/tt>')</>
</group>



<?INSTED COMMENT: GROUP ul>

<group name="ul">
	<left-indent>	+=10	</>
	<space-before>	14	</>
	<break-before>	Line	</>
	<text-before><ul></>
	<text-after>join('<','/ul>')</>
</group>

<style name="BULLETLIST" group="ul">
	<space-after>	0	</>
</style>

<style name="BULLETLISTIND" group="ul">
	<left-indent>	&list.left-indent	</>
</style>



<?INSTED COMMENT: GROUP var>

<group name="var">
	<font-slant>	Italics	</>
	<text-before><var></>
	<text-after>join('<','/var>')</>
</group>

<style name="VARIABLE" group="var">
	<font-family>	&body.font-family	</>
	<font-slant>	Italics	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>



<?INSTED COMMENT: UNGROUPED STYLES FOLLOW>

<style name="#DEFAULT">
	<font-family>	times	</>
	<font-weight>	Medium	</>
	<font-slant>	Roman	</>
	<font-video>	Regular	</>
	<font-size>	14	</>
	<line-spacing>	17	</>
</style>

<style name="#FOOTER">
	<text-before><B>join(content(typechild(TITLE,typechild(TITLEPAGE,typechild(FRONTMATTER,ancestor(MANUAL))))),'<','/B><P><HR><I>Maintained by <A HREF="http:','/','/','sgi.com>Maintainer<','/A>',': Send e-mail to <A HREF="mailto:','name','@','address">name','@','address.<','/I>')</>
</style>

<style name="#HEADER">
	<text-before>join(<'IMG SRC="/images/SGI_ID.gif" ALIGN="right" alt="Silicon Graphics" WIDTH=151 HEIGHT=43>','<','p>')</>
	<text-after>join('<','P>')</>
</style>

<style name="#QUERY">
	<font-video>	Inverse	</>
	<foreground>	gray55	</>
</style>

<style name="#ROOT">
	<break-before>	Line	</>
</style>

<style name="#SDATA">
	<text-before>   switch(attr(name),trade,'(TM)',mdash,'-',DEFAULT,join('&',attr(name),';'))       </>
</style>

<style name="#TAGS">
	<font-size>	-=4	</>
	<foreground>	purple	</>
</style>

<style name="APPENDIX">
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="APPENDIX,TITLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
	<font-size>	&title.text.b4-size	</>
	<line-spacing>	20	</>
	<break-after>	True	</>
</style>

<style name="AUDIO">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	sound	</>
</style>

<style name="BULLET">
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="BULLET,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETIND">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	true	</>
</style>

<style name="BULLETIND,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETLIST,BULLET">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="BULLETSQUAREIND">
	<vertical-offset>	0	</>
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="BULLETSQUAREIND,CODE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BULLETSQUAREIND,PARAGRAPH">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="CALLOUT">
	<font-family>	helvetica	</>
</style>

<style name="CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<space-before>	switch(istrue(ancestor(or(CODE,EXAMPLE))),TRUE,0,DEFAULT,default.ind.space-b4)	</>
	<break-before>	True	</>
	<break-after>	Line	</>
	<text-before><p></>
	<column>	False	</>
</style>

<style name="CAUTION,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="CELL,PARAGRAPH,FIGURE">
	<left-indent>	&left-indent.1	</>
</style>

<style name="CELL,PARAGRAPH,FIGURE,GRAPHIC">
	<select>	if(eq(file(env(HOME)/.figsInsight),FILE),TBL_GRAPHIC_OUT,TBL_GRAPHIC_IN)	</>
</style>

<style name="CELL_PARAGRAPH">
	<space-before>	if(eq(cnum(),1),0,default.space-b4)	</>
	<break-before>	True	</>
	<text-before>if(and(isfirst,ancestor(TABLEHEADING)),,<p>)</>
	<text-after>if(isempty(content(me())),'<br><br>','')</>
</style>

<style name="CHAPTER">
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="CHAPTER,TITLE,#TEXT-BEFORE">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
	<font-size>	&title.text.b4-size	</>
	<line-spacing>	20	</>
	<space-after>	0	</>
	<break-after>	True	</>
</style>

<style name="CHECKOFFITEM">
	<space-before>	&default.ind.space-b4	</>
	<text-before><p>__ </>
</style>

<style name="CHECKOFFITEM,#TEXT-BEFORE">
	<character-set>	symbol	</>
	<break-before>	true	</>
</style>

<style name="CODE">
	<font-family>	&command-font	</>
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'LIST',+=18,'LISTIND',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
	<text-before>if(typechild(CAPTION),,<PRE>)</>
	<text-after>join('<','/PRE>')</>
</style>

<style name="CODE,CAPTION">
	<text-before><P></>
	<text-after><PRE></>
</style>

<style name="CREDITSPAGE">
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="CREDITSPAGE,PARAGRAPH">
	<break-after>	True	</>
	<text-before><P></>
</style>

<style name="DATE">
	<break-after>	True	</>
</style>

<style name="DEFINITION">
	<hide>	All	</>
</style>

<style name="DOCNUMBER">
	<font-family>	helvetica	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="ERRORMSG">
	<text-before><dl></>
	<text-after>join('<','/dl>')</>
</style>

<style name="EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'LIST',+=18,'LISTIND',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
	<text-before>if(typechild(CAPTION),,<pre>)</>
	<text-after>join('<','/pre>')</>
</style>

<style name="EXAMPLE,CAPTION">
	<text-before><p></>
	<text-after><pre></>
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

<style name="EXPLANATION">
	<left-indent>	&hang.left-indent	</>
	<break-before>	true	</>
	<text-before><dd></>
	<text-after>join('<','/dd>')</>
</style>

<style name="EXTPROGRAM">
	<foreground>	&cross-link.color	</>
	<left-indent>	40	</>
	<script>	ebt-launch cmd="attr(APP) attr(PARMS)"	</>
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
	<first-indent>	-=30	</>
	<break-before>	True	</>
	<text-after><p><br></>
</style>

<style name="FIGURE,CAPTION,PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
	<font-size>	&body.font-size	</>
	<text-before><B></>
	<text-after>join(':  <','/B>'</>
</style>

<style name="FTNOTE">
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
	<script>	ebt-reveal  window="new" stylesheet="fulltext.rev" hscroll="false" width=450 title="Footnote"	</>
	<icon-type>	footnote	</>
</style>

<style name="GENERALINFO">
	<space-before>	&default.space-b4	</>
</style>

<style name="GLOSSARY">
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYENTRY">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<first-indent>	&gloss.first-indent	</>
	<space-before>	8	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before><dt></>
	<text-after>join('<','/dt>')</>
</style>

<style name="GLOSSARYITEM">
	<score>	Under	</>
	<space-after>	0	</>
	<script>	sgi-glossary window=new book=glossary stylesheet=fulltext.rev root="'parent(query(<GLOSSARYENTRY> containing 'content(me())'))'"	</>
</style>

<style name="GLOSSARYTERM">
	<left-indent>	&gloss.left-indent	</>
	<break-before>	True	</>
	<text-before><dl></>
	<text-after>join('<','/dl>')</>
</style>

<style name="GRAPHIC">
	<inline>	if(contains(attr(FILE),.cgm),,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILE)"	</>
	<text-before><p>if(contains(attr(FILE),.cgm),CANNOT DISPLAY GRAPHIC<br>,)</>
</style>

<style name="GRAPHIC_IN">
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<inline>	if(contains(attr(FILE),.cgm),vector,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILE)"	</>
</style>

<style name="GRAPHIC_OUT">
	<foreground>	&hot-link.color	</>
	<left-indent>	-=10	</>
	<icon-position>	Left	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE).hot),FILE),rasterhot,raster)	</>
</style>

<style name="HANGBODY">
	<left-indent>	&hang.left-indent	</>
	<break-before>	if(ancestor(REFERENCE),if(eq(max(12,length(content(lsibling()))),12), false,true),false)	</>
	<break-after>	False	</>
</style>

<style name="HANGPAIR">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="HANGPAIR,PARAGRAPH">
	<break-before>	True	</>
</style>

<style name="HANGPAIRIND">
	<break-after>	True	</>
</style>

<style name="HEADER">
	<space-before>	&default.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
</style>

<style name="HINT,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
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
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="KEYWORDS">
	<break-after>	True	</>
</style>

<style name="LAUNCHWORD">
	<foreground>	&cross-link.color	</>
	<hide>	Children	</>
	<script>	ebt-launch cmd="attr(APP) attr(PARMS)"	</>
	<text-before> if(contains(attr(PARMS),http),join('<A HREF="',attr(PARMS),'">',content(me()),'<','/A>'),content(me()))</>
</style>

<style name="LIST,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTIND">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="LISTIND,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
	<text-before>if(isfirst(ancestor()), if(contains(tag(ancestor()),IND),<br><li><p>,<li><p>),  if(contains(tag(nchild(-1,lsibling(ancestor()))),IND),<br><li><p>, if(or(contains(tag(nchild(-1,lsibling(ancestor()))),EXAMPLE),contains(tag(nchild(-1,lsibling(ancestor()))),CODE)),<li><p>, if(eq(TABLE,tag(nchild(-1,lsibling(ancestor())))),<li><p>,<br><br><li><p>))))</>
</style>

<style name="MANUAL">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<foreground>	&default.foreground	</>
	<left-indent>	&left-indent.1	</>
	<right-indent>	&right-indent.1	</>
	<first-indent>	&left-indent.1	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	False	</>
	<break-after>	True	</>
	<column>	False	</>
</style>

<style name="MANUAL,FRONTMATTER">
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<space-before>	10	</>
	<icon-position>	Left	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="MARGINTEXT">
	<font-slant>	Italics	</>
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<space-before>	0	</>
	<hide>	Children	</>
	<script>	ebt-reveal  window="new" stylesheet="fulltext.rev" hscroll="false" width=450 title="Margin Note"	</>
	<text-before>Margin Note<p></>
</style>

<style name="MARGINTEXT,#TEXT-BEFORE">
	<font-slant>	Italics	</>
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<space-before>	0	</>
	<icon-position>	Left	</>
	<break-after>	TRUE	</>
	<script>	ebt-reveal  window="new" stylesheet="fulltext.rev" hscroll="false" width=450 title="Margin Note"	</>
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

<style name="MSG">
	<font-family>	courier	</>
	<space-before>	&default.space-b4	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
	<text-before>if(isfirst(),<dt><pre>,)</>
	<text-after>if(islast(),join('<','/pre><','/dt>'),)</>
</style>

<style name="NAMES">
	<font-weight>	Medium	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before>if(isfirst(),join('<','/B><P>'),<P>)</>
</style>

<style name="NOTE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="ORDEREDLIST,LIST">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
</style>

<style name="PARAGRAPH">
	<select>	if(eq(tag(ancestor()),CELL),CELL_PARAGRAPH,NORMAL_PARAGRAPH)	</>
</style>

<style name="PART">
	<space-before>	&chp.title.space-b4	</>
</style>

<style name="PART,PARAGRAPH">
	<select>	if(eq(tag(ancestor()),CELL),CELL_PARAGRAPH,NORMAL_PARAGRAPH)	</>
</style>

<style name="PART,TITLE,#TEXT-BEFORE">
	<font-family>	new century schoolbook	</>
	<font-weight>	Bold	</>
	<font-slant>	Roman	</>
	<font-size>	11	</>
	<space-after>	10	</>
	<break-after>	True	</>
</style>

<style name="PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
	<font-size>	&body.font-size	</>
	<text-before><B></>
	<text-after>join(': <','/B>')</>
</style>

<style name="PROGRAMNAME">
	<font-slant>	Italics	</>
</style>

<style name="QANDA">
	<space-before>	&default.space-b4	</>
</style>

<style name="REFENTRY">
	<break-before>	True	</>
	<text-before>join('<H1>',attr(TITLE,typechild(REFNAME)),'<','/H1>')</>
</style>

<style name="REFERENCE">
	<space-before>	if(ancestor(APPENDIX),0,36)	</>
	<break-before>	True	</>
</style>

<style name="REFNAME">
	<space-before>	if(isfirst(),6,0)	</>
	<hide>	Children	</>
	<break-before>	if(lsibling(),false,true)	</>
</style>

<style name="REFNAMEDIV">
	<text-before>NAME</>
</style>

<style name="REFPURPOSE">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<space-before>	10	</>
	<break-before>	True	</>
</style>

<style name="REFSECT1,TITLE">
	<break-before>	True	</>
	<text-before><H3></>
	<text-after>join('<','/H3>')</>
</style>

<style name="REFSECT2,TITLE">
	<break-before>	True	</>
	<text-before><H4></>
	<text-after>join('<','/H4>')</>
</style>

<style name="REFSECT3,TITLE">
	<break-before>	True	</>
	<text-before><H5></>
	<text-after>join('<','/H5>')</>
</style>

<style name="ROW">
	<font-family>	new century schoolbook	</>
	<space-before>	if(ancestor(CAPTION),if(isfirst(),0,4),if(isfirst(),4,6))	</>
	<space-after>	if(lsibling(TABLEHEADING,ancestor(TABLEBODY)),if(islast(),6,0),0)	</>
	<!--	<space-before>	&table.space-before	</> -->
	<break-before>	True	</>
	<text-before><TR ALIGN=LEFT VALIGN=TOP></>
	<text-after>if(islast(),join('<','/TR>'),join('<','/TR>'))</>
</style>

<style name="SCREENDISPLAY">
	<font-family>	&command-font	</>
	<text-before>if(or(ancestor(CODE),ancestor(EXAMPLE)),,<TT>)</>
	<text-after>if(or(ancestor(CODE),ancestor(EXAMPLE)),,join('<','/TT>'))</>
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

<style name="SECTION4">
	<break-before>	True	</>
</style>

<style name="SGIDOC">
	<text-before><html> </>
	<text-after>join('<','/body>','<','/html>')</>
</style>

<style name="SGIINDEX">
	<hide>	All	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<text-before>&index.text-b4 </>
</style>

<style name="SGIINDEX,#TEXT-BEFORE">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
</style>

<style name="SP">
	<break-before>	False	</>
	<break-after>	False	</>
	<text-before>/></>
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
	<font-size>	16	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="SSBBODY,PARAGRAPH">
	<font-family>	&wp.font-family	</>
	<font-size>	16	</>
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
	<font-weight>	Medium	</>
	<character-set>	symbol	</>
	<hide>	Children	</>
	<text-before>switch(content(),Ô,'(TM)',ë,'[',û,']',â,®,'DEFAULT',content())</>
</style>

<style name="SYNOPSIS">
	<space-before>	if(isfirst(),6,3)	</>
	<break-before>	true	</>
</style>

<style name="TABLE">
	<text-before><TABLE if(typechild(CAPTION),BORDER,) CELLPADDING=10>if(typechild(CAPTION),<p>,)</>
	<text-after>join('<','/TABLE>') if(typechild(CAPTION),<br><br>,)</>
</style>

<style name="TABLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&hot-link.color	</>
	<space-before>	&default.space-b4	</>
	<script>	ebt-reveal stylesheet="fulltext.v"	</>
</style>

<style name="TABLE,CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<space-before>	6	</>
	<space-after>	if(rsibling(),6,0)	</>
	<text-before><CAPTION ALIGN=TOP></>
	<text-after>join('<','/CAPTION>')</>
	<column>	False	</>
</style>

<style name="TABLE,CAPTION,PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
	<font-size>	&body.font-size	</>
	<text-before><B></>
	<text-after>join(':  <','/B>')</>
</style>

<style name="TABLEBODY">
	<hrule>	if (lsibling(TABLEHEADING),after,)	</>
</style>

<style name="TABLEFOOTNOTE">
	<font-family>	new century schoolbook	</>
	<left-indent>	55	</>
	<first-indent>	-7	</>
	<space-before>	3	</>
	<break-before>	Line	</>
	<text-before>attr(LBL)</>
</style>

<style name="TABLEFOOTNOTE,#TEXT-BEFORE">
	<font-size>	-=2	</>
	<vertical-offset>	4	</>
</style>

<style name="TABLEHEADING">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
	<score>	None	</>
	<space-after>	4	</>
	<hrule>	Surround	</>
	<vrule>	None	</>
	<break-before>	True	</>
	<text-before><TR VALIGN=TOP ALIGN=LEFT></>
</style>

<style name="TABLEXREF">
	<foreground>	&hot-link.color	</>
	<vertical-offset>	4	</>
	<script>	ebt-reveal root=idmatch(ID,attr(IDREF)) window="new" stylesheet="fulltext.rev" hscroll="true" title="Table Footnote" width=500	</>
</style>

<style name="TABLE_IN">
	<font-size>	&body.font-size	</>
	<left-indent>	if(typechild(CAPTION),48,)	</>
	<width>	mult(1.55,add(attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)))), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),2)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),3)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),4)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),5)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),6)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),7)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),8)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),9)), attr(WIDTH,typechild(CELL,typechild(ROW,typechild(TABLEBODY)),10))))	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	if(ancestor(REFERENCE),12,if(rsibling(typechild(CAPTION)),0,if(typechild(CAPTION),12,0)))	</>
	<space-after>	if(typechild(CAPTION),6,0)	</>
	<break-before>	true	</>
</style>

<style name="TABLE_OUT">
	<foreground>	&hot-link.color	</>
	<left-indent>	-=10	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
	<script>	ebt-reveal stylesheet=fulltext.rev hscroll=true window=new width=700	</>
	<icon-type>	table	</>
</style>

<style name="TBL_GRAPHIC_IN">
	<left-indent>	if(eq(cnum(ancestor(CELL)),1),48,int(add(48,mult(1.45,attr(LEFT,ancestor(CELL))))))	</>
	<space-before>	6	</>
	<space-after>	6	</>
	<break-before>	true	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<inline>	if(contains(attr(FILE),.cgm),vector,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILE)"	</>
</style>

<style name="TBL_GRAPHIC_OUT">
	<foreground>	&hot-link.color	</>
	<icon-position>	Inline	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE).hot),FILE),rasterhot,raster)	</>
</style>

<style name="TIP,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
	<text-before>if(or(ancestor(CODE),ancestor(EXAMPLE)),<B>,<TT><B>)</>
	<text-after>if(or(ancestor(CODE),ancestor(EXAMPLE)),join('<','/B>'),join('<','/B><','/TT>'))</>
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
</style>

<style name="XREF">
	<font-weight>	Bold	</>
	<foreground>	&hot-link.color	</>
	<select>	XREF,XREF_attr(TYPE)	</>
</style>

<style name="XREF_">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="XREF_GRAPHIC">
	<script>	ebt-link target=typechild(GRAPHIC,ancestor(ancestor(idmatch(ID,attr(IDREF)))))	</>
</style>

<style name="XREF_TABLE">
	<script>	ebt-link target=typechild(CAPTION,ancestor(ancestor(idmatch(ID,attr(IDREF)))))	</>
</style>

<style name="XREF_TEXT">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="XREF_TITLE">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="ZAPF">
	<font-family>	itc zapf dingbats	</>
	<character-set>	symbol	</>
</style>



</sheet>
