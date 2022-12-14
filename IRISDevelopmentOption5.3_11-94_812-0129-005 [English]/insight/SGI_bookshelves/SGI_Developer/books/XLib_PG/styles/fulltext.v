<!-- Version $Revision: 1.3 $ of the fulltext stylesheet -->

<!ENTITY	atable.space-b4	CDATA	"12"	>
<!ENTITY	body.font-family	CDATA	"new century schoolbook"	>
<!ENTITY	body.font-size	CDATA	"12"	>
<!ENTITY	body.line-space	CDATA	"14"	>
<!ENTITY	bullet-color	CDATA	"grey30"	>
<!ENTITY	bullet.font-family	CDATA	"symbol"	>
<!ENTITY	bullet.font-size	CDATA	"14"	>
<!ENTITY	bullet.left-indent	CDATA	"+=30"	>
<!ENTITY	bullpara.space-b4	CDATA	"4"	>
<!ENTITY	chp.title.font-size	CDATA	"18"	>
<!ENTITY	chp.title.line-space	CDATA	"20"	>
<!ENTITY	chp.title.space-b4	CDATA	"5"	>
<!ENTITY	command-font	CDATA	"courier"	>
<!ENTITY	comment.left-indent	CDATA	"30"	>
<!ENTITY	comment.right-indent	CDATA	"5"	>
<!ENTITY	cross-link.color	CDATA	"#96000d"	>
<!ENTITY	default.foreground	CDATA	"grey20"	>
<!ENTITY	default.space-after	CDATA	"6"	>
<!ENTITY	default.space-b4	CDATA	"6"	>
<!ENTITY	doc.title.font-size	CDATA	"24"	>
<!ENTITY	doc.title.line-space	CDATA	"26"	>
<!ENTITY	gloss.first-indent	CDATA	"-20"	>
<!ENTITY	gloss.lbl.font-size	CDATA	"14"	>
<!ENTITY	gloss.lbl.line-space	CDATA	"18"	>
<!ENTITY	gloss.lbl.space-bef	CDATA	"20"	>
<!ENTITY	gloss.left-indent	CDATA	"50"	>
<!ENTITY	gloss.space-after	CDATA	"10"	>
<!ENTITY	hang.left-indent	CDATA	"+=90"	>
<!ENTITY	hot-link.color	CDATA	"#000078"	>
<!ENTITY	item.left-indent	CDATA	"+=25"	>
<!ENTITY	left-indent.1	CDATA	"12"	>
<!ENTITY	note.first-indent	CDATA	"-=36"	>
<!ENTITY	note.foreground	CDATA	"black"	>
<!ENTITY	note.space-aft	CDATA	"10"	>
<!ENTITY	note.space-before	CDATA	"10"	>
<!ENTITY	order.left-indent	CDATA	"+=15"	>
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
<!ENTITY	title-font	CDATA	"new century schoolbook"	>
<!ENTITY	title.left-indent	CDATA	"-6"	>
<!ENTITY	title.text.b4-lsp	CDATA	"+=2"	>
<!ENTITY	title.text.b4-size	CDATA	"14"	>
<!ENTITY	var.left-indent	CDATA	"+=60"	>
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

<style name="LISTITEM,CAUTION" group="note-caut-warn">
	<left-indent>	+=75	</>
	<first-indent>	-=55	</>
	<text-before>Caution:</>
</style>

<style name="LISTITEM,NOTE" group="note-caut-warn">
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

<style name="APPENDIX,SECT1,SECT2,SECT3,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec2.space-before	</>
	<text-before>format(cnum(parent(APPENDIX)),LETTER).cnum(parent(SECT1)).cnum(parent(SECT2)).cnum(parent(SECT3))  </>
</style>

<style name="APPENDIX,SECT1,SECT2,TITLE" group="titles">
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before>format(cnum(parent(APPENDIX)),LETTER).cnum(parent(SECT1)).cnum(parent(SECT2))  </>
</style>

<style name="APPENDIX,SECT1,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>format(cnum(parent(APPENDIX)),LETTER).cnum(parent(SECT1))  </>
</style>

<style name="APPENDIX,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>if(isempty(attr(NUMBER,ancestor(APPENDIX))),'',Appendix attr(NUMBER,ancestor(APPENDIX)))</>
</style>

<style name="CHAPTER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
	<text-before>if(isempty(attr(number,ancestor(CHAPTER))),'',Chapter attr(number,ancestor(CHAPTER)))</>
</style>

<style name="GLOSSARY,SECT1,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
</style>

<style name="GLOSSARY,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="INTRODUCTION,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
</style>

<style name="PREFACE,SECT1,SECT2,TITLE" group="titles">
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
</style>

<style name="PREFACE,SECT1,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>if(isempty(attr(LBL,ancestor(SECTION1))),'',join(attr(LBL,ancestor(SECTION1)),'   '))</>
</style>

<style name="REFNAME" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	20	</>
</style>

<style name="REFSECT1,TITLE" group="titles">
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	5	</>
</style>

<style name="REFSECT2,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec2.space-before	</>
</style>

<style name="REFSECT3,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec4.space-before	</>
</style>

<style name="SECT1,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&sec1.space-before	</>
	<text-before>cnum(ancestor(CHAPTER)).cnum(parent(SECT1))   </>
</style>

<style name="SECT2,TITLE" group="titles">
	<font-size>	&sec1.title.font-size	</>
	<line-spacing>	&sec1.line-spacing	</>
	<space-before>	&sec2.space-before	</>
	<text-before>cnum(ancestor(CHAPTER)).cnum(ancestor(SECT1)).cnum(parent(SECT2))     </>
</style>

<style name="SECT3,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec2.space-before	</>
	<text-before>cnum(ancestor(CHAPTER)).cnum(ancestor(SECT1)).cnum(parent(SECT2)).cnum(parent(SECT3))  </>
</style>

<style name="SECT4,TITLE" group="titles">
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&sec4.space-before	</>
	<text-before>cnum(ancestor(CHAPTER)).cnum(ancestor(SECT1)).cnum(parent(SECT2)).cnum(parent(SECT3)).cnum(parent(SECT4))  </>
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
	<space-after>	30	</>
</style>

<style name="SUPPORT,TITLEPAGE,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="TITLEPAGE,TITLE" group="titles">
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="WHITEPAPER,TITLE" group="titles">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<space-before>	&wp.space-before	</>
</style>



<?INSTED COMMENT: UNGROUPED STYLES FOLLOW>

<style name="#QUERY">
	<font-video>	Inverse	</>
	<foreground>	gray55	</>
</style>

<style name="#ROOT">
	<break-before>	Line	</>
</style>

<!-- Default settings for SDATA entities from our entity sets -->
<style name="#SDATA">
	<font-family>	attr(font)	</>
	<font-weight>	Medium	</>
	<font-slant>	Roman	</>
	<character-set>	attr(charset)	</>
	<break-before>	None	</>
	<text-before>char(attr(code))</>
</style>

<style name="#TAGS">
	<font-size>	8	</>
	<foreground>	purple	</>
	<line-spacing>	10	</>
</style>

<style name="APPENDIX">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="APPENDIX,EXAMPLE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<justification>	Left	</>
	<break-before>	True	</>
	<text-before>if(eq(length(attr(ID,ancestor(EXAMPLE))),11),Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),11).  ,Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),12).  )</>
</style>

<style name="APPENDIX,REFENTRY,REFSECT1,FIGURE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-after>	&default.space-after	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILEREF,rsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILEREF,rsibling(GRAPHIC))" title="content(#TEXT-BEFORE)content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),empty,rasterhot),empty)	</>
	<text-before>if(eq(length(attr(ID,ancestor(FIGURE))),11),Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),11)  ,Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,rsibling(GRAPHIC)),17)  )</>
</style>

<style name="APPENDIX,SECT1,EXAMPLE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Italics	</>
	<text-before>if(eq(length(attr(ID,ancestor(EXAMPLE))),11),Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),11).  ,Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),12).  )</>
</style>

<style name="APPENDIX,SECT1,FIGURE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-after>	&default.space-after	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILEREF,rsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILEREF,rsibling(GRAPHIC))" title="content(#TEXT-BEFORE)content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),empty,rasterhot),empty)	</>
	<text-before>if(eq(length(attr(ID,ancestor(FIGURE))),11),Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),11).  ,Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),12).  )</>
</style>

<style name="APPENDIX,SECT1,SECT2,EXAMPLE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Italics	</>
	<text-before>if(eq(length(attr(ID,ancestor(EXAMPLE))),11),Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),11).  ,Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),12).  )</>
</style>

<style name="APPENDIX,SECT1,SECT2,FIGURE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-after>	&default.space-after	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILEREF,rsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILEREF,rsibling(GRAPHIC))" title="content(#TEXT-BEFORE)content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),empty,rasterhot),empty)	</>
	<text-before>if(eq(length(attr(ID,ancestor(FIGURE))),11),Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),11).  ,Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),12).  )</>
</style>

<style name="APPENDIX,SECT1,SECT2,SECT3,EXAMPLE,TITLE">
	<text-before>if(eq(length(attr(ID,ancestor(EXAMPLE))),11),Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),11).  ,Example format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(EXAMPLE)),12).  )</>
</style>

<style name="APPENDIX,SECT1,SECT2,SECT3,FIGURE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-after>	&default.space-after	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILEREF,rsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILEREF,rsibling(GRAPHIC))" title="content(#TEXT-BEFORE)content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),empty,rasterhot),empty)	</>
	<text-before>if(eq(length(attr(ID,ancestor(FIGURE))),11),Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),11).  ,Figure format(cnum(ancestor(APPENDIX)),LETTER)-substr(attr(ID,ancestor(FIGURE)),12).  )</>
</style>

<style name="APPENDIX,TITLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
	<font-size>	&title.text.b4-size	</>
	<break-after>	True	</>
</style>

<style name="AUDIO">
	<foreground>	&hot-link.color	</>
	<icon-position>	Right	</>
	<script>	ebt-launch cmd="attr(APP) attr(FILE)"	</>
	<icon-type>	sound	</>
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

<style name="BOOKTITLE">
	<foreground>	black	</>
</style>

<style name="BULLET">
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>?</>
</style>

<style name="BULLET,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="BULLET,EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BULLET,PARA">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLET,PROGRAMLISTING">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
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

<style name="BULLETIND,PARA">
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
	<text-before>?</>
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

<style name="BULLETSQUAREIND,NOTE">
	<left-indent>	+=54	</>
	<first-indent>	-=37	</>
	<space-before>	&note.space-before	</>
	<space-after>	&note.space-aft	</>
	<break-before>	True	</>
	<text-before>Note:</>
</style>

<style name="BULLETSQUAREIND,PARA">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="BULLETSQUAREIND,PROGRAMLISTING">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="BUTTON">
	<font-slant>	Italics	</>
</style>

<style name="CAPTION">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<left-indent>	&left-indent.1	</>
	<space-before>	4	</>
	<break-before>	True	</>
	<text-before>content(child(PREFIX))</>
	<column>	False	</>
</style>

<style name="CAPTION,#TEXT-BEFORE">
	<font-family>	helvetica	</>
	<font-weight>	Bold	</>
</style>

<style name="CAUTION,#TEXT-BEFORE">
	<font-weight>	Bold	</>
	<foreground>	&note.foreground	</>
</style>

<style name="CELL">
	<left-indent>	if(eq(cnum(),1),12,int(mult(1.4,attr(LEFT))))	</>
	<width>	int(mult(1.27,attr(WIDTH)))	</>
	<column>	True	</>
</style>

<style name="CELL,PARAGRAPH,SCREENDISPLAY">
	<break-after>	None	</>
</style>

<style name="CELL_PARAGRAPH">
	<space-before>	if(eq(cnum(),1),0,^\default.space-b4)	</>
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

<style name="CITEBOOK">
	<font-slant>	Italics	</>
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

<style name="COMPUTEROUTPUT">
	<font-family>	&command-font	</>
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

<style name="CREDITSPAGE">
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="CREDITSPAGE,PARA">
	<hide>	Children	</>
	<break-after>	True	</>
</style>

<style name="DATE">
	<break-after>	True	</>
</style>

<style name="DEFINITION">
	<hide>	All	</>
</style>

<style name="DOCBOOK">
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

<style name="DOCBOOK,DOCINFO">
	<foreground>	&hot-link.color	</>
	<space-before>	10	</>
	<icon-position>	Right	</>
	<break-before>	True	</>
	<break-after>	True	</>
	<script>	ebt-link root=me() window=new stylesheet=frontmatter	</>
	<icon-type>	copyrt	</>
</style>

<style name="DOCNUMBER">
	<space-before>	&chp.title.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="EDITOR">
	<hide>	Children	</>
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
	<space-before>	if(eq(file(env(HOME)/.figsInsight),file),0,6)	</>
	<space-after>	if(eq(file(env(HOME)/.figsInsight),file),0,6)	</>
</style>

<style name="EXAMPLE,PROGRAMLISTING">
	<font-family>	&command-font	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="EXAMPLE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<justification>	Left	</>
	<break-before>	True	</>
	<text-before>if(eq(length(attr(ID,ancestor(EXAMPLE))),11),Example cnum(ancestor(CHAPTER))-substr(attr(ID,ancestor(EXAMPLE)),11).  ,Example cnum(ancestor(CHAPTER))-substr(attr(ID,ancestor(EXAMPLE)),12).  )</>
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
	<space-before>	if(eq(file(env(HOME)/.figsInsight),file),2,6)	</>
	<space-after>	if(eq(file(env(HOME)/.figsInsight),file),2,6)	</>
	<break-before>	True	</>
</style>

<style name="FIGURE,TITLE">
	<font-family>	helvetica	</>
	<font-slant>	Oblique	</>
	<font-size>	&body.font-size	</>
	<foreground>	&hot-link.color	</>
	<space-after>	&default.space-after	</>
	<icon-position>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),Off,Right),Off)	</>
	<break-before>	True	</>
	<script>	ebt-if(contains(attr(FILEREF,rsibling(GRAPHIC)),.cgm),vector,raster) filename="attr(FILEREF,rsibling(GRAPHIC))" title="content(#TEXT-BEFORE)content(me())"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF,rsibling(GRAPHIC)).hot),file),if(eq(file(env(HOME)/.figsInsight),file),empty,rasterhot),empty)	</>
	<text-before>if(eq(length(attr(ID,ancestor(FIGURE))),11),Figure cnum(ancestor(CHAPTER))-substr(attr(ID,ancestor(FIGURE)),11).  ,Figure cnum(ancestor(CHAPTER))-substr(attr(ID,ancestor(FIGURE)),12).  )</>
</style>

<style name="FILENAME">
	<font-slant>	Italics	</>
</style>

<style name="FIRSTNAME">
	<hide>	Children	</>
</style>

<style name="FOOTNOTE">
	<left-indent>	10	</>
	<right-indent>	0	</>
	<width>	350	</>
	<icon-position>	Left	</>
	<hide>	Children	</>
	<script>	ebt-reveal  window="new" stylesheet="fulltext.v" hscroll="false" width=450 title="Footnote"	</>
	<icon-type>	footnote	</>
</style>

<style name="FUNCTION">
	<font-family>	courier	</>
	<font-weight>	Bold	</>
	<font-size>	13	</>
</style>

<style name="GENERALINFO">
	<space-before>	&default.space-b4	</>
</style>

<style name="GLOSSARY">
	<space-before>	&sec1.space-before	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYDEF">
	<left-indent>	+=40	</>
	<space-after>	&gloss.space-after	</>
	<break-before>	True	</>
</style>

<style name="GLOSSARYENTRY">
	<font-weight>	Medium	</>
	<first-indent>	&gloss.first-indent	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="GLOSSARYITEM">
	<score>	Under	</>
	<script>	sgi-glossary window=new book=glossary root="'parent(query(<GLOSSARYENTRY> containing 'content(me())'))'"	</>
</style>

<style name="GLOSSARYTERM">
	<font-weight>	Bold	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="GLOSSARYTERM,EMPHASIS">
</style>

<style name="GRAPHIC">
	<select>	if(eq(file(env(HOME)/.figsInsight),file),GRAPHIC_OUT,GRAPHIC_IN)	</>
</style>

<style name="GRAPHIC_IN">
	<script>	ebt-if(contains(attr(FILEREF),.cgm),vector,raster) filename="attr(FILEREF)" title="content(lsibling('TITLE'))"	</>
	<inline>	if(contains(attr(FILEREF),.cgm),vector,raster) scale=if(isempty(attr(SCALE)),FALSE,attr(SCALE)) filename="attr(FILEREF)" title="content(lsibling('TITLE'))	</>
</style>

<style name="GRAPHIC_OUT">
	<foreground>	&hot-link.color	</>
	<space-before>	-20	</>
	<icon-position>	Right	</>
	<script>	ebt-if(contains(attr(FILEREF),.cgm),vector,raster) filename="attr(FILEREF)" title="content(lsibling('TITLE'))"	</>
	<icon-type>	if(eq(file(var(fig_dir)/attr(FILEREF).hot),file),rasterhot,raster)	</>
</style>

<style name="HANGBODY">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="HANGBODY,BULLETLISTIND,BULLETIND">
	<left-indent>	&hang.left-indent	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>?</>
</style>

<style name="HANGBODY,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="HANGBODY,ORDEREDLISTIND,LISTIND">
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="HANGBODY,PARA">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="HANGBODY,PROGRAMLISTING">
	<font-family>	&command-font	</>
	<left-indent>	103	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="HANGBODYIND,PARA">
	<left-indent>	&hang.left-indent	</>
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
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
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="HANGPAIR,PARA">
	<break-before>	True	</>
</style>

<style name="HANGPAIRIND">
	<break-after>	True	</>
</style>

<style name="HARDWARE">
	<font-weight>	Bold	</>
</style>

<style name="HEADER">
	<space-before>	&default.space-b4	</>
	<hide>	Children	</>
	<break-before>	True	</>
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
	<hide>	All	</>
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

<style name="ISBN">
	<hide>	Children	</>
</style>

<style name="ITALICS">
	<font-slant>	Italics	</>
</style>

<style name="ITEMIZEDLIST">
	<space-after>	0	</>
</style>

<style name="ITEMIZEDLIST,LISTITEM">
	<vertical-offset>	0	</>
	<space-before>	&bullpara.space-b4	</>
	<break-before>	True	</>
	<text-before>?</>
</style>

<style name="ITEMIZEDLIST,LISTITEM,ITEMIZEDLIST,LISTITEM">
	<left-indent>	+=26	</>
	<vertical-offset>	0	</>
	<space-before>	&bullpara.space-b4	</>
	<break-before>	True	</>
	<text-before>-</>
</style>

<style name="ITEMIZEDLIST,LISTITEM,PARA">
	<select>	ITEMPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="ITEMPARA*FIRST*FALSE">
	<left-indent>	+=25	</>
	<break-before>	True	</>
</style>

<style name="ITEMPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&item.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
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

<style name="LEGALNOTICE">
	<font-weight>	Bold	</>
	<hide>	Children	</>
	<break-before>	True	</>
	<break-after>	True	</>
</style>

<style name="LIST,BULLETLISTIND,BULLETIND">
	<left-indent>	&bullet.left-indent	</>
	<vertical-offset>	0	</>
	<space-after>	0	</>
	<break-before>	True	</>
	<break-after>	False	</>
	<text-before>?</>
</style>

<style name="LIST,BULLETLISTIND,BULLETIND,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="LIST,PROGRAMLISTING">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="LISTIND,PARA">
	<select>	BULLETPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTITEM">
	<left-indent>	&bullet.left-indent	</>
	<vertical-offset>	0	</>
	<space-before>	&bullpara.space-b4	</>
	<break-before>	True	</>
	<text-before>?</>
</style>

<style name="LISTITEM,#TEXT-BEFORE">
	<font-family>	&bullet.font-family	</>
	<font-size>	&bullet.font-size	</>
	<character-set>	symbol	</>
	<foreground>	&bullet-color	</>
</style>

<style name="LISTITEM,EXAMPLE">
	<font-family>	&command-font	</>
	<left-indent>	&bullet.left-indent	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="LISTITEM,PARA">
	<select>	ITEMPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="LISTITEM,PARA,FOOTNOTE">
	<left-indent>	0	</>
	<icon-position>	Right	</>
	<hide>	Children	</>
	<script>	ebt-reveal stylesheet=fulltext.v	</>
	<icon-type>	footnote	</>
</style>

<style name="LISTITEM,SCREEN">
	<font-family>	&command-font	</>
	<left-indent>	+=25	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="LITERAL">
	<font-family>	courier	</>
	<font-weight>	Bold	</>
	<font-size>	13	</>
</style>

<style name="LITERALLAYOUT">
	<justification>	Verbatim	</>
	<break-before>	True	</>
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

<style name="NONPRKEYS">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
</style>

<style name="NORMAL_PARAGRAPH">
	<space-before>	&default.space-b4	</>
	<space-after>	5	</>
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

<style name="ORDEREDLIST,LISTITEM">
	<space-before>	&default.space-b4	</>
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="ORDEREDLIST,LISTITEM,PARA">
	<select>	ORDERPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="ORDEREDLISTIND,LISTIND">
	<left-indent>	&bullet.left-indent	</>
	<break-before>	True	</>
	<text-before>cnum(me()).</>
</style>

<style name="ORDERPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&order.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="PARA">
	<select>	if(eq(tag(ancestor()),CELL),CELL_PARAGRAPH,NORMAL_PARAGRAPH)	</>
</style>

<style name="PREFACE,TITLE">
	<font-weight>	Bold	</>
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hrule>	Before	</>
</style>

<style name="PREFIX">
	<font-family>	&table-font	</>
	<font-weight>	Bold	</>
	<font-size>	&body.font-size	</>
	<hide>	Text	</>
</style>

<style name="PRINTHISTORY">
	<hide>	Children	</>
</style>

<style name="PROGRAMLISTING">
	<font-family>	&command-font	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="PROGRAMLISTING,COMPUTEROUTPUT">
	<font-family>	&command-font	</>
	<break-before>	Line	</>
</style>

<style name="PROGRAMLISTING,USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
</style>

<style name="PROGRAMNAME">
	<font-slant>	Italics	</>
</style>

<style name="PROMPT">
	<break-before>	True	</>
</style>

<style name="PUBLISHER">
	<hide>	Children	</>
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

<style name="REFMETA">
	<hide>	Children	</>
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

<style name="SCREEN">
	<font-family>	&command-font	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="SCREEN,PROMPT">
	<break-before>	True	</>
</style>

<style name="SECT1">
	<break-before>	True	</>
</style>

<style name="SECT2">
	<break-before>	True	</>
</style>

<style name="SECT3">
	<break-before>	True	</>
</style>

<style name="SECT4">
	<break-before>	True	</>
</style>

<style name="SERIES">
	<foreground>	black	</>
	<hide>	Children	</>
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

<style name="SIDEBAR">
	<space-before>	10	</>
	<space-after>	10	</>
	<break-before>	True	</>
</style>

<style name="SIDEBAR,PARA">
	<font-slant>	Italics	</>
	<left-indent>	20	</>
	<right-indent>	20	</>
	<space-after>	5	</>
	<break-after>	True	</>
</style>

<style name="SIDEBAR,PARA,TITLE">
	<font-slant>	Roman	</>
	<font-size>	18	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="SIDEBAR,TITLE">
	<font-weight>	Bold	</>
	<font-size>	18	</>
	<left-indent>	20	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="SP">
	<break-before>	False	</>
	<break-after>	False	</>
	<text-before>? ?</>
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

<style name="SSLIST,LISTIND,PARA">
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="SUBSCRIPT">
	<vertical-offset>	-3	</>
</style>

<style name="SUPERSCRIPT">
	<font-size>	10	</>
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

<style name="SURNAME">
	<hide>	Children	</>
</style>

<style name="SYM">
	<font-family>	symbol	</>
	<character-set>	symbol	</>
</style>

<style name="SYM,#TEST-BEFORE">
</style>

<style name="SYMBOL">
	<font-family>	symbol	</>
	<font-weight>	Medium	</>
	<character-set>	symbol	</>
</style>

<style name="SYNOPSIS">
	<font-family>	courier	</>
	<break-before>	True	</>
</style>

<style name="SYNOPSIS,#TEXT-BEFORE">
	<font-family>	&title-font	</>
	<font-weight>	Bold	</>
	<font-size>	&sec1.title.font-size	</>
	<space-before>	15	</>
	<space-after>	5	</>
	<break-before>	True	</>
</style>

<style name="TABLE">
	<font-family>	&table-font	</>
	<font-size>	&body.font-size	</>
	<line-spacing>	&body.line-space	</>
	<space-before>	&default.space-b4	</>
	<space-after>	&default.space-after	</>
</style>

<style name="TABLEFOOTNOTE">
	<icon-position>	Right	</>
	<hide>	Children	</>
	<script>	ebt-reveal stylesheet=fulltext.v	</>
	<icon-type>	footnote	</>
</style>

<style name="TABLEHEADING">
	<font-weight>	Bold	</>
	<score>	Under	</>
	<break-before>	True	</>
</style>

<style name="TERM">
	<line-spacing>	&body.line-space	</>
	<break-before>	True	</>
</style>

<style name="TITLE">
	<font-weight>	Bold	</>
	<font-size>	&doc.title.font-size	</>
	<line-spacing>	&doc.title.line-space	</>
	<space-after>	&chp.title.space-b4	</>
</style>

<style name="TITLE,#TEXT-BEFORE">
	<font-weight>	Bold	</>
</style>

<style name="TITLEABBREV">
	<font-size>	&chp.title.font-size	</>
	<line-spacing>	&chp.title.line-space	</>
	<hide>	Text	</>
	<text-before>if(isempty(attr(LBL,ancestor(CHAPTER))),'',Chapter attr(LBL,ancestor(CHAPTER)))</>
</style>

<style name="USERINPUT">
	<font-family>	&command-font	</>
	<font-weight>	Bold	</>
</style>

<style name="VARIABLE">
	<font-slant>	Italics	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="VARLISTENTRY">
	<space-before>	5	</>
</style>

<style name="VARLISTENTRY,LISTITEM">
	<select>	if(le(sub(length(content(lsibling(TERM))),3),10),sameline,nextline)	</>
</style>

<style name="VARLISTENTRY,LISTITEM,PARA">
	<select>	VARPARA*FIRST*eq(1,cnum())	</>
</style>

<style name="VARLISTENTRY,LISTITEM,SCREEN">
	<font-family>	&command-font	</>
	<left-indent>	+=60	</>
	<justification>	Verbatim	</>
	<break-before>	True	</>
</style>

<style name="VARLISTENTRY,LISTITEM,VARIABLELIST">
	<left-indent>	60	</>
</style>

<style name="VARLISTENTRY,LISTITEM,VARIABLELIST,VARLISTENTRY,LISTITEM">
	<left-indent>	180	</>
</style>

<style name="VARPARA*FIRST*FALSE">
	<left-indent>	&var.left-indent	</>
	<space-before>	4	</>
	<break-before>	True	</>
</style>

<style name="VARPARA*FIRST*TRUE">
	<font-size>	&body.font-size	</>
	<left-indent>	&var.left-indent	</>
	<break-before>	False	</>
	<break-after>	False	</>
</style>

<style name="VARPARAM">
	<font-family>	courier	</>
	<font-weight>	Bold	</>
	<font-slant>	Italics	</>
</style>

<style name="VBLOCK">
	<break-before>	True	</>
</style>

<style name="XREF">
	<font-weight>	Bold	</>
	<foreground>	&hot-link.color	</>
	<script>	ebt-link target=idmatch(ID,attr(LINKEND))	</>
</style>

<style name="XREF_GRAPHIC">
	<script>	ebt-if(contains(attr(FILE,lsibling(GRAPHIC,ancestor(CAPTION,idmatch(ID,attr(IDREF))))),.cgm),vector,raster) filename="attr(FILE,lsibling(GRAPHIC,ances	</>
</style>



</sheet>
