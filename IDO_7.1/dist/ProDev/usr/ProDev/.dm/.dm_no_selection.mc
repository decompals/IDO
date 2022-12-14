#!/bin/sh
# MetaCard 1.4 stack
# The following is not ASCII text,
# so now would be a good time to q out of more
exec mc $0 "$@"
                                                                                                                              aHome,/usr1/metacard/mc/mchome
MetaCard Menu Bar,/usr1/metacard/mc/mctools
Help Index,/usr1/metacard/mc/mchelp
Demo,mcdemo
Message Box,mctools
Dialog Box Help,mchelp
MetaCard Tutorials,mchelp
MetaTalk Reference,mchelp
Support,mchelp
MetaCard Version,mchelp
Licensing MetaCard,mchelp
License Form,mchelp
Demo Sounds,mcsounds
MetaCard FAQ,mchelp
image_lib,/usr1/metacard/magic/.database
intro,/usr1/metacard/magic/.intro.mc
MetaCard Tutorial,mchelp
magic_bak,/usr1/metacard/magic/magic.bak.1
Stack 780069259,/var/tmp/cami/cami/cami
mike_stack,/usr1/metacard/mike/mike.05
mike_stack03,/usr1/metacard/mike/mike.03
App_Menu,/usr1/metacard/mike/app_menu.01
Copy of Enabled_Selected_Menu,/usr1/metacard/mike/app_menu.01
desktop_menu,/usr1/metacard/magic/desktop_menu.mc
magic_stack,/usr1/metacard/magic/.magic.mc
find_menu,/usr1/metacard/magic/find_menu.mc
customize_menu,/usr1/metacard/magic/customize_menu.mc
exit_option,/usr1/metacard/magic/.exit_option.mc
glossary_option,/usr1/metacard/magic/.glossary_option.mc
language_option,/usr1/metacard/magic/.language_option.mc
audio_option,/usr1/metacard/magic/.audio_option.mc
magic_lesson1,/usr1/metacard/magic/.lesson1.mc
lesson1,/usr1/metacard/magic/lesson1.mc
magic_lesson2,/usr1/metacard/magic/.lesson2.mc
magic_lesson3,/usr1/metacard/magic/.lesson3.mc
magic_lesson5,/usr1/metacard/magic/.lesson5.mc
magic_lesson4,/usr1/metacard/magic/.lesson4.mc
magic_lesson6,/usr1/metacard/magic/.lesson6.mc
magic_help,/usr1/metacard/magic/.no_selection.mc
magic_lesson7,/usr1/metacard/magic/.lesson7.mc
Enabled_Selected_Menu,/usr1/metacard/mike/app_menu.01
test_lesson7,/usr1/metacard/magic/test_lesson7.mc
dm_no_selection,/usr1/metacard/dm/.dm_no_selection.mc
dm_overview,/usr1/metacard/dm/.dm_overview.mc
DevMagic,/usr/metacard/gallery/.cbt_gallery.mc
cbt_gallery,/usr/metacard/gallery/.cbt_gallery.mc
cbt_delivered,/usr/metacard/gallery/.cbt_delivered.mc
extra,/usr/metacard/gallery/.cbt_extra.mc
cbt_right,/usr/metacard/gallery/.cbt_right.mc
dm_intro,/usr1/metacard/dm/.dm_intro.mc
overview.mc,/usr/metacard/cv/overview.mc
debugger.mc,/usr/metacard/cv/overview.mc
Stack 791765627,Stack 791765627.mc
dm_performance_analyzer,/usr/metacard/cv/dm_performance_analyzer.mc
dm_per_analyzer,/usr/metacard/cv/dm_per_analyzer.mc
dm_overview.mc,/usr/metacard/cv/dm_overview.bak
dm_howto.mc,/usr/metacard/cv/dm_howto
dm_debugger.mc,/usr/metacard/cv/dm_debugger.mc
dm_static_analyzer,/usr/metacard/cv/dm_static_analyzer.mc
dm_map,/usr1/metacard/cv/.map_option.mc
dm_stat_analyzer.mc,/usr1/metacard/dm/.dm_stat_analyzer.mc
dm_perf_analzer.mc,/usr1/metacard/dm/.dm_perf_analzer.mc
Stack 791776929,/usr/metacard/cv/.dm_howto.mc
dm_howto,/usr1/metacard/dm/.dm_howto.mc
error_file,/usr1/metacard/magic/.magic/.error_file.mc
dm_debug.mc,/usr1/metacard/cv/.dm_debug.mc
RapidApp Introduction,/usr1/metacard/cv/rapid/RapidAppIntro
workshopstack.test,/usr1/metacard/cv/workshopstack.5
dm_howto1.mc,/usr1/metacard/cv/.dm_howto1.mc
dev_magic,/usr1/metacard/cv/.dev_magic.mc
dm_glossary,/usr1/metacard/dm/.dm_glossary.mc
dm_audio,/usr1/metacard/dm/.dm_audio.mc
dm_mainmenu,/usr1/metacard/dm/.dm_mainmenu.mc
no_selection,/usr1/metacard/magic/.no_selection.mc
   ? dm_no_selection # 	p?on resetLesson2
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 1 to 15
    put 1 into char i of @LESSON_PLAN
  end repeat

end resetLesson2


on resetLesson3
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 2 to 17
    put 1 into char i of @LESSON_PLAN
   end repeat

end resetLesson3


on resetLesson4
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 2 to 14
    put 1 into char i of @LESSON_PLAN
   end repeat

end resetLesson4

on resetLesson5
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 2 to 18
    put 1 into char i of @LESSON_PLAN
   end repeat

end resetLesson5


on resetLesson6
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 2 to 12
    put 1 into char i of @LESSON_PLAN
   end repeat

end resetLesson6


on resetLesson7
  global @LESSON_PLAN

#skip customization screen
  put 0 into char 1 of @LESSON_PLAN

#pre-set lesson_plan to be for all objectives
  repeat with i = 2 to 13
    put 1 into char i of @LESSON_PLAN
   end repeat

end resetLesson7

on mouseUp
  
end mouseUp
        ?9? 	KEQUMBWX        Must Make Selection            U 
Helvetica   U 
Helvetica  W 
Helvetica   W 
Helvetica  W 
Helvetica  U 
Helvetica  "W 
Helvetica  W 
Helvetica  U 
Helvetica  U Times   U Times   W Times   U Times   W Times  " W Times   ? no_selection   	P on openCard
  
end openCard
                 ?  ?  ?  ?  ?
  ? Selection_options     -  ????    ??????     < Qj ?            ? 1You have not selected any objectives to learn.      	You Can:   ? entire ?d?x   ?on mouseUp
  global @DO_OBJ

  repeat with i = 1 to 13
     put 1 into char i of @DO_OBJ
  end repeat

  close this stack
end mouseUp
  7 ??????  ??  ??    ????  ??????  cc  cc     ?LP ,     Take Entire Course         	  ? 	       ? custom ?d?x   +on mouseUp
  close this stack
end mouseUp
  7 ??????  ??  ??    ????  ??????  cc  cc     ? ?P ,     Customize Lesson         	  ? 	       ? mm ?d?x   ?on mouseUp
  go to stack  ".dm_mainmenu"
  close stack ".dm_howto.mc"
  close stack ".dm_overview.mc"
#  close stack ".dm_static.mc"
#  close stack ".dm_debug.mc"
#  close stack ".dm_perf.mc"

  close stack "dm_no_selection"
end mouseUp
  7 ??????  ??  ??    ????  ??????  cc  cc     ??P ,     Go to Main Menu         	  ? 	      