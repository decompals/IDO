<!-- Version $Revision: 1.99 $ of the fulltext stylesheet -->

<!ENTITY	app.text-b4	CDATA	"if(isempty(attr(LBL,ancestor(APPENDIX))),'',switch(attr(LANG,ancestor(SGIDOC)),de,Anhang ,fr,Appendice ,ja_JP.EUC,�t�^ ,DEFAULT,Appendix ) attr(LBL,ancestor(APPENDIX)))"	>
<!ENTITY	body.font-family	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,mincho,DEFAULT,new century schoolbook)"	>
<!ENTITY	body.font-size	CDATA	"12"	>
<!ENTITY	body.line-space	CDATA	"17"	>
<!ENTITY	bullet-color	CDATA	"grey30"	>
<!ENTITY	bullet.font-family	CDATA	"symbol"	>
<!ENTITY	bullet.font-size	CDATA	"14"	>
<!ENTITY	bullet.left-indent	CDATA	"+=18"	>
<!ENTITY	bullpara.space-b4	CDATA	"6"	>
<!ENTITY	character-set	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,*,DEFAULT,)"	>
<!ENTITY	chp.text-b4	CDATA	"if(isempty(attr(LBL,ancestor(CHAPTER))),'',switch(attr(LANG,ancestor(SGIDOC)),de,Kapitel ,fr,Chapitre ,ja_JP.EUC,�� ,DEFAULT,Chapter ) attr(LBL,ancestor(CHAPTER))) if(isempty(attr(LBL,ancestor(CHAPTER))),'',switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC, ��,DEFAULT,))"	>
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
<!ENTITY	font-slant.italics	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,*,DEFAULT,Italics)"	>
<!ENTITY	font-slant.roman	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,*,DEFAULT,Roman)"	>
<!ENTITY	font-weight.bold	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,Medium,DEFAULT,Bold)"	>
<!ENTITY	font-weight.medium	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,Medium,DEFAULT,Medium)"	>
<!ENTITY	gloss.first-indent	CDATA	"32"	>
<!ENTITY	gloss.font-family	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,gothic,DEFAULT,helvetica)"	>
<!ENTITY	gloss.lbl.font-size	CDATA	"14"	>
<!ENTITY	gloss.lbl.line-space	CDATA	"20"	>
<!ENTITY	gloss.lbl.space-bef	CDATA	"20"	>
<!ENTITY	gloss.left-indent	CDATA	"18"	>
<!ENTITY	gloss.space-after	CDATA	"0"	>
<!ENTITY	hang.left-indent	CDATA	"+=100"	>
<!ENTITY	hot-link.color	CDATA	"#000078"	>
<!ENTITY	index.text-b4	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),de,Index,fr,Index,ja_JP.EUC,����,DEFAULT,Index)"	>
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
<!ENTITY	table-font	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,gothic,DEFAULT,helvetica)"	>
<!ENTITY	tag.font-family	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,gothic,DEFAULT,helvetica)"	>
<!ENTITY	title	CDATA	"black"	>
<!ENTITY	title-font	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,mincho,DEFAULT,new century schoolbook)"	>
<!ENTITY	title.font-family	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,gothic,DEFAULT,helvetica)"	>
<!ENTITY	title.left-indent	CDATA	"-6"	>
<!ENTITY	title.text.b4-lsp	CDATA	"0"	>
<!ENTITY	title.text.b4-size	CDATA	"13"	>
<!ENTITY	wp.font-family	CDATA	"switch(attr(LANG,ancestor(SGIDOC)),ja_JP.EUC,mincho,DEFAULT,screen)"	>
<!ENTITY	wp.font-size	CDATA	"12"	>
<!ENTITY	wp.line-spacing	CDATA	"14"	>
<!ENTITY	wp.space-before	CDATA	"40"	>

<sheet >



<?INSTED COMMENT: GROUP #TAGS>

<group name="#TAGS">
	<font-family>	&tag.font-family	</>
	<font-weight>	&font-weight.medium	</>
	<font-size>	*	</>
	<foreground>	purple	</>
	<score>	Under	</>
</group>



<?INSTED COMMENT: GROUP cells>

<group name="cells">
	<left-indent>	if(eq(cnum(),1),48,int(add(48,mult(1.45,attr(LEFT)))))	</>
	<width>	int(mult(1.45,attr(WIDTH)))	</>
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



<?INSTED COMMENT: GROUP note-caut-warn>

<group name="note-caut-warn">
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'BULLETSQUAREIND',+=18,'LIST',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
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
	<font-weight>	&font-weight.bold	</>
	<icon-position>	Left	</>
	<hide>	Off	</>
	<script>	ebt-link FALSE	</>
	<icon-type>	warn	</>
</style>



<?INSTED COMMENT: GROUP titles>

<group name="titles">
	<font-family>	&title.font-family	</>
	<font-weight>	&font-weight.bold	</>
	<character-set>	&character-set	</>
	<foreground>	&title	</>
	<justification>	Left	</>
	<break-before>	True	</>
</group>

<style name="APPENDIX,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>&app.text-b4</>
</style>

<style name="CHAPTER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<left-indent>	48	</>
	<first-indent>	0	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>&chp.text-b4</>
</style>

<style name="GLOSSARY,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="HELPTOPIC,TITLE" group="titles">
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<hide>	All	</>
	<break-before>	if(ancestor(REFERENCE),true,false)	</>
</style>

<style name="INTRODUCTION,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="PART,TITLE" group="titles">
	<font-size>	22	</>
	<line-spacing>	26	</>
	<space-before>	6	</>
	<hrule>	Before	</>
	<text-before>PART switch(attr(LBL,ancestor(PART)),I,ONE,II,TWO,III,THREE,IV,FOUR,V,FIVE,VI,SIX,VII,SEVEN,VIII,EIGHT,IX,NINE,DEFAULT,TEN)</>
</style>

<style name="SECTION1,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="SECTION2,TITLE" group="titles">
	<font-size>	18	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION2))),'',join(attr(LBL,ancestor(SECTION2)),'   '))</>
</style>

<style name="SECTION3,TITLE" group="titles">
	<font-size>	14	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec3.space-before	</>
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
	<left-indent>	48	</>
	<line-spacing>	&chp.title.line-space	</>
</style>

<style name="SUPPORT,TITLEPAGE,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
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
</style>



<?INSTED COMMENT: UNGROUPED STYLES FOLLOW>

<style name="#QUERY">
	<font-video>	Inverse	</>
	<foreground>	gray55	</>
</style>

<style name="#ROOT">
	<character-set>	&character-set	</>
	<break-before>	Line	</>
</style>

<style name="#SDATA">
	<font-family>	attr(font)	</>
	<font-weight>	&font-weight.medium	</>
	<font-slant>	&font-slant.roman	</>
	<character-set>	attr(charset)	</>
	<text-before>char(attr(code))</>
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
	<font-weight>	&font-weight.bold	</>
	<font-slant>	&font-slant.italics	</>
	<font-size>	&title.text.b4-size	</>
	<line-spacing>	20	</>
	<break-after>	True	</>
</style>

<style name="ARGUMENT">
	<font-family>	&body.font-family	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="AUDIO">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	sound	</>
</style>

<style name="BNF">
	<font-slant>	&font-slant.italics	</>
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
	<font-family>		</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="BULLET">
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>�</>
</style>

<style name="BULLET,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLET,PARAGRAPH">
	<break-before>	None	</>
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETIND">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	true	</>
	<text-before>if(tag(ancestor(BULLET)),-,�)</>
</style>

<style name="BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETIND,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
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
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<break-after>	true	</>
	<text-before>�</>
</style>

<style name="BULLETLIST,BULLET,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETLISTIND">
	<left-indent>	&list.left-indent	</>
</style>

<style name="BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&body.font-family	</>
	<font-size>	&bullet.font-size	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLETSQUAREIND">
	<vertical-offset>	0	</>
	<space-before>	&default.ind.space-b4	</>
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

<style name="BULLETSQUAREIND,PARAGRAPH">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BUTTON">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="CALLOUT">
	<font-family>	&tag.font-family	</>
</style>

<style name="CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<space-before>	switch(istrue(ancestor(or(CODE,EXAMPLE))),TRUE,0,DEFAULT,default.ind.space-b4)	</>
	<break-before>	True	</>
	<break-after>	Line	</>
	<column>	False	</>
</style>

<style name="CAUTION,#TEXT-BEFORE">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="CELL,PARAGRAPH,FIGURE">
	<line-spacing>	17	</>
	<space-before>	6	</>
	<break-after>	None	</>
</style>

<style name="CELL,PARAGRAPH,FIGURE,GRAPHIC">
	<select>	if(eq(file(env(HOME)/.figsInsight),FILE),TBL_GRAPHIC_OUT,TBL_GRAPHIC_IN)	</>
</style>

<style name="CELL_PARAGRAPH">
	<space-before>	if(eq(cnum(),1),0,default.space-b4)	</>
	<break-before>	None	</>
	<break-after>	True	</>
</style>

<style name="CHAPTER">
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="CHAPTER,TITLE,#TEXT-BEFORE">
	<font-family>	&title.font-family	</>
	<font-weight>	&font-weight.bold	</>
	<font-slant>	&font-slant.italics	</>
	<font-size>	&title.text.b4-size	</>
	<line-spacing>	20	</>
	<space-after>	0	</>
	<break-after>	True	</>
</style>

<style name="CHECKOFFITEM">
	<space-before>	&default.ind.space-b4	</>
	<text-before>o </>
</style>

<style name="CHECKOFFITEM,#TEXT-BEFORE">
	<font-family>	&sqbullet.font-family	</>
	<font-size>	14	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
	<break-before>	true	</>
</style>

<style name="CMDLINEOPT">
	<font-weight>	&font-weight.bold	</>
</style>

<style name="CODE">
	<font-family>	&command-font	</>
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'LIST',+=18,'LISTIND',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="COMMAND">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="COMMENT">
	<font-slant>	&font-slant.italics	</>
	<left-indent>	&comment.left-indent	</>
	<right-indent>	&comment.right-indent	</>
	<break-before>	True	</>
</style>

<style name="CONTRIBUTORS">
	<font-weight>	&font-weight.bold	</>
	<space-before>	&chp.title.space-b4	</>
	<hide>	Children	</>
</style>

<style name="COORDINATE">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="COPYRIGHT">
	<font-weight>	&font-weight.bold	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
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
	<font-family>	&tag.font-family	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="DOCTITLE">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="DUMMY">
	<space-before>	6	</>
</style>

<style name="EMPHASIS">
	<font-slant>	&font-slant.italics	</>
	<hide>	if(ancestor(TABLE,ancestor(CAPTION)),ALL,)	</>
	<script>	if(ancestor(TABLE,ancestor(CAPTION)),ALL,)	</>
</style>

<style name="EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	switch(tag(ancestor()),'BULLET',+=18,'BULLETIND',+=18,'LIST',+=18,'LISTIND',+=18,'DEFAULT',+=0)	</>
	<space-before>	&default.ind.space-b4	</>
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

<style name="EXPLANATION">
	<left-indent>	&hang.left-indent	</>
	<break-before>	true	</>
</style>

<style name="EXTPROGRAM">
	<foreground>	&cross-link.color	</>
	<left-indent>	40	</>
	<icon-position>	Left	</>
	<script>	ebt-launch cmd="attr(APP) attr(PARMS)"	</>
	<icon-type>	extpgm	</>
</style>

<style name="EXTREF">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&cross-link.color	</>
	<left-indent>	switch(attr(INFO),3rd_pty,40,DEFAULT,)	</>
	<icon-position>	switch(attr(INFO),3rd_pty,Left,DEFAULT,Off)	</>
	<script>	ebt-link book=attr(BOOK) tname="ID" tvalue=attr(IDREF)	</>
	<icon-type>	switch(attr(INFO),3rd_pty,sgilink,DEFAULT,)	</>
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
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILE,lsibling(GRAPHIC)).hot),FILE),if(eq(file(env(HOME)/.figsInsight),FILE),Off,Left),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILE,lsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILE,lsibling(GRAPHIC))" title="content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE,lsibling(GRAPHIC)).hot),FILE),if(eq(file(env(HOME)/.figsInsight),FILE),empty,rasterhot),empty)	</>
</style>

<style name="FIGURE,CAPTION,PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	&font-weight.bold	</>
	<font-size>	&body.font-size	</>
	<script>	ebt-if(contains(attr(FILE,typechild(GRAPHIC,ancestor(FIGURE))),.cgm),vector,raster) filename="attr(FILE,typechild(GRAPHIC,ancestor(FIGURE)))" title="content(me())"	</>
	<icon-type>	empty	</>
	<text-after>if(ancestor(CAPTION),,: ) </>
</style>

<style name="FILENAME">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="FTNOTE">
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
	<script>	ebt-reveal  window="new" stylesheet="fulltext.rev" hscroll="false" width=450 title="Footnote"	</>
	<icon-type>	footnote	</>
</style>

<style name="FUNCTION">
	<font-family>	&body.font-family	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="GENERALINFO">
	<space-before>	&default.space-b4	</>
</style>

<style name="GLOSSARY">
	<space-before>	&chp.title.space-b4	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYDEF">
	<left-indent>	&left-indent.1	</>
	<space-after>	&gloss.space-after	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYENTRY">
	<font-family>	&gloss.font-family	</>
	<font-weight>	&font-weight.bold	</>
	<space-before>	8	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="GLOSSARYITEM">
	<score>	Under	</>
	<space-after>	0	</>
	<script>	sgi-glossary window=new book=glossary stylesheet=fulltext.rev root="'parent(query(<GLOSSARYENTRY> containing 'content(me())'))'"	</>
</style>

<style name="GLOSSARYTERM">
	<icon-position>	Off	</>
	<break-before>	True	</>
</style>

<style name="GRAPHIC">
	<icon-position>	Left	</>
	<select>	if(eq(file(env(HOME)/.figsInsight),FILE),GRAPHIC_OUT,GRAPHIC_IN)	</>
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

<style name="HANGBODY,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="HANGBODY,PARAGRAPH">
	<line-spacing>	&body.line-space	</>
	<break-before>	if(lsibling(PARAGRAPH),True,False)	</>
	<break-after>	True	</>
</style>

<style name="HANGBODYIND,PARAGRAPH">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	False	</>
	<break-after>	True	</>
</style>

<style name="HANGITEM">
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
	<break-after>	False	</>
</style>

<style name="HANGLIST">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="HANGLISTIND">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
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

<style name="HARDWARELABEL">
	<font-family>	&tag.font-family	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="HEADER">
	<space-before>	&default.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
</style>

<style name="HINT,#TEXT-BEFORE">
	<font-weight>	&font-weight.bold	</>
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

<style name="ITALICS">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="KEYWORD">
	<font-family>	&body.font-family	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="KEYWORDS">
	<break-after>	True	</>
</style>

<style name="LABEL">
	<font-weight>	&font-weight.bold	</>
	<font-size>	&gloss.lbl.font-size	</>
	<line-spacing>	&gloss.lbl.line-space	</>
	<space-before>	&gloss.lbl.space-bef	</>
</style>

<style name="LAUNCHWORD">
	<foreground>	&cross-link.color	</>
	<script>	ebt-launch cmd="attr(APP) attr(PARMS)"	</>
</style>

<style name="LIST,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="LIST,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTIND">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>cnum(me()).</>
</style>

<style name="LISTIND,PARAGRAPH">
	<select>	LISTPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTPARA*FIRST*FALSE">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
</style>

<style name="LISTPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&bullet.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
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
	<script>	ebt-link root=me() window=new stylesheet=frontmatter	</>
	<icon-type>	copyrt	</>
</style>

<style name="MARGINTEXT">
	<font-slant>	&font-slant.italics	</>
	<foreground>	&hot-link.color	</>
	<left-indent>	40	</>
	<space-before>	0	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
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
</style>

<style name="NAMES">
	<font-weight>	&font-weight.medium	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="NEWLINE">
	<line-spacing>	13	</>
	<break-before>	if (ancestor(TITLE), false, true)	</>
</style>

<style name="NEWTERM">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="NONPRKEYS">
	<font-family>	&command-font	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="NORMAL_PARAGRAPH">
	<space-before>	if(isfirst(),6,switch(tag(ancestor(*,me(),1)), PART,12,INTRODUCTION,12,CHAPTER,12,APPENDIX,12,SECTION1,12,SECTION2,12,SECTION3,12,DEFAULT,6))	</>
	<break-before>	True	</>
</style>

<style name="NOTE,#TEXT-BEFORE">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="ORDEREDLIST">
	<space-after>	5	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="ORDEREDLIST,LIST">
	<space-before>	&default.ind.space-b4	</>
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="ORDEREDLISTIND">
	<left-indent>	&list.left-indent	</>
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
	<font-family>	&title-font	</>
	<font-weight>	&font-weight.bold	</>
	<font-slant>	&font-slant.roman	</>
	<font-size>	11	</>
	<space-after>	10	</>
	<break-after>	True	</>
</style>

<style name="PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	&font-weight.bold	</>
	<font-size>	&body.font-size	</>
	<text-after>if(ancestor(CAPTION),,: ) </>
</style>

<style name="PROGRAMNAME">
	<font-slant>	&font-slant.italics	</>
</style>

<style name="QANDA">
	<space-before>	&default.space-b4	</>
</style>

<style name="REFERENCE">
	<space-before>	if(ancestor(APPENDIX),0,36)	</>
	<break-before>	True	</>
</style>

<style name="REFPAGE">
	<foreground>	&cross-link.color	</>
	<icon-position>	Off	</>
	<script>	ebt-launch cmd="/usr/sbin/xwsh -title \'content(me())\' -geom 80x40 -hold -e man if(eq(words(translate(translate(content(me()),'(',' '),')',' ')),2), join(strip(word(translate(translate(content(me()),'(',' '),')',' '),2),'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz','R'),' ',word(translate(translate(content(me()),'(',' '),')',' '),1)), content(me()))"	</>
</style>

<style name="ROW">
	<font-family>		</>
	<space-before>	if(ancestor(CAPTION),if(isfirst(),0,4),if(isfirst(),4,6))	</>
	<space-after>	if(lsibling(TABLEHEADING,ancestor(TABLEBODY)),if(islast(),6,0),0)	</>
	<!--	<space-before>	&table.space-before	</> -->
	<break-before>	True	</>
</style>

<style name="SCREENDISPLAY">
	<font-family>	&command-font	</>
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
	<text-before>� �</>
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
	<font-weight>	&font-weight.medium	</>
	<character-set>	symbol	</>
</style>

<style name="SYNOPSIS">
	<space-before>	if(isfirst(),6,3)	</>
	<break-before>	true	</>
</style>

<style name="TABLE">
	<icon-position>	Left	</>
	<select>	if(typechild(CAPTION),if(eq(file(env(HOME)/.tabsInsight),FILE),TABLE_OUT,TABLE_IN),)	</>
</style>

<style name="TABLE,#TEXT-BEFORE">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&hot-link.color	</>
	<space-before>	&default.space-b4	</>
	<script>	ebt-reveal stylesheet="fulltext.v"	</>
</style>

<style name="TABLE,CAPTION">
	<font-family>	&body.font-family	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-before>	6	</>
	<space-after>	if(rsibling(),6,0)	</>
	<script>	ebt-reveal stylesheet="fulltext.rev" title="change(content(me()),' (continued)','',false)" hscroll="true" root=ancestor(TABLE) window="new" lockhdrs="true" tablehdr=typechild(TABLEHEADING,ancestor(TABLE)) tablebody=typechild(TABLEBODY,ancestor(TABLE))  width=mult(2.12,add(12,attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),1)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),2)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),3)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),4)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),5)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),6)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),7)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),8)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),9)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),10))))	</>
	<column>	False	</>
</style>

<style name="TABLE,CAPTION,PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	&font-weight.bold	</>
	<font-size>	&body.font-size	</>
	<script>	ebt-reveal stylesheet="fulltext.rev" title="change(content(typechild(CAPTION,ancestor(TABLE))),' (continued)','',false)" hscroll="true" root=ancestor(TABLE) window="new" lockhdrs="true" tablehdr=typechild(TABLEHEADING,ancestor(TABLE)) tablebody=typechild(TABLEBODY,ancestor(TABLE))  width=mult(2.12,add(12,attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),1)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),2)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),3)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),4)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),5)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),6)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),7)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),8)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),9)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,ancestor(TABLE)),10))))	</>
	<text-after>if(ancestor(CAPTION),,: ) </>
</style>

<style name="TABLEBODY">
	<hrule>	if (lsibling(TABLEHEADING),after,)	</>
</style>

<style name="TABLEFOOTNOTE">
	<font-family>		</>
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
	<font-family>	&title.font-family	</>
	<font-weight>	&font-weight.bold	</>
	<score>	None	</>
	<space-after>	4	</>
	<hrule>	Surround	</>
	<vrule>	None	</>
	<break-before>	True	</>
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
	<space-before>	if(rsibling(typechild(CAPTION)),0,if(typechild(CAPTION),12,0))	</>
	<space-after>	if(typechild(CAPTION),6,6)	</>
	<break-before>	true	</>
	<break-after>	true	</>
</style>

<style name="TABLE_OUT">
	<foreground>	&hot-link.color	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
	<break-before>	TRUE	</>
	<script>	ebt-reveal stylesheet="fulltext.rev" title="content(typechild(CAPTION))" hscroll="true" root=me() window="new" lockhdrs="true" tablehdr=typechild(TABLEHEADING) tablebody=typechild(TABLEBODY)  width=mult(2.12,add(12,attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),1)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),2)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),3)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING,),4)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),5)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),6)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),7)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),8)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),9)), attr(WIDTH,typechild(CELL,typechild(TABLEHEADING),10))))	</>
	<icon-type>	table	</>
	<text-after>content(typechild(CAPTION))</>
</style>

<style name="TABLE_OUT,#TEXT-AFTER">
	<script>	ebt-reveal stylesheet=fulltext.rev hscroll=true window=new width=700	</>
</style>

<style name="TBL_GRAPHIC_IN">
	<break-before>	None	</>
	<break-after>	None	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<inline>	if(contains(attr(FILE),.cgm),vector,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILE)"	</>
</style>

<style name="TBL_GRAPHIC_OUT">
	<foreground>	&hot-link.color	</>
	<icon-position>	Inline	</>
	<script>	ebt-if(contains(attr(FILE),.cgm),vector,raster) filename="attr(FILE)" title="content(rsibling('CAPTION'))"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILE).hot),FILE),rasterhot,raster)	</>
</style>

<style name="TECHNICAL">
	<font-weight>	&font-weight.bold	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="TIP,#TEXT-BEFORE">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	&font-weight.bold	</>
</style>

<style name="VARIABLE">
	<font-family>	&body.font-family	</>
	<font-slant>	&font-slant.italics	</>
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
	<font-weight>	&font-weight.bold	</>
</style>

<style name="XREF">
	<font-weight>	&font-weight.bold	</>
	<foreground>	&hot-link.color	</>
	<select>	XREF,XREF_attr(TYPE)	</>
</style>

<style name="XREF_">
	<script>	ebt-link target=idmatch(ID,attr(IDREF))	</>
</style>

<style name="XREF_GRAPHIC">
	<script>	ebt-if(contains(attr(FILE,lsibling(GRAPHIC,ancestor(CAPTION,idmatch(ID,attr(IDREF))))),.cgm),vector,raster) filename="attr(FILE,lsibling(GRAPHIC,ancestor(CAPTION,idmatch(ID,attr(IDREF)))))" title="join(change(content(ancestor(CAPTION,idmatch(ID,attr(IDREF)))),\",\'\',TRUE))"	</>
</style>

<style name="XREF_TABLE">
	<script>	ebt-link target=ancestor(ancestor(idmatch(ID,attr(IDREF))))	</>
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
