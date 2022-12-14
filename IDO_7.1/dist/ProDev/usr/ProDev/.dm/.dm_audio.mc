#!/bin/sh
# MetaCard 1.4 stack
# The following is not ASCII text,
# so now would be a good time to q out of more
exec mc $0 "$@"
                                                                                                                              ?Home,/usr1/metacard/mc/mchome
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
dm_audio,/usr1/metacard/cv/.audio_option.mc
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
no_selection,/usr1/metacard/magic/.no_selection.mc
dm_overview,/usr1/metacard/cv/.dm_overview.mc
DevMagic,/usr/metacard/gallery/.cbt_gallery.mc
cbt_gallery,/usr/metacard/gallery/.cbt_gallery.mc
cbt_delivered,/usr/metacard/gallery/.cbt_delivered.mc
extra,/usr/metacard/gallery/.cbt_extra.mc
cbt_right,/usr/metacard/gallery/.cbt_right.mc
dm_mainmenu,/usr1/metacard/cv/.dev_magic.mc
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
dm_stat_analyzer.mc,/usr1/metacard/cv/.dm_stat_analyzer.mc
dm_perf_analzer.mc,/usr1/metacard/cv/.dm_perf_analzer.mc
Stack 791776929,/usr/metacard/cv/.dm_howto.mc
dm_howto,/usr1/metacard/cv/.dm_howto1.mc
error_file,/usr1/metacard/magic/.magic/.error_file.mc
dm_debug.mc,/usr1/metacard/cv/.dm_debug.mc
RapidApp Introduction,/usr1/metacard/cv/rapid/RapidAppIntro
workshopstack.test,/usr1/metacard/cv/workshopstack.5
dm_howto1.mc,/usr1/metacard/cv/.dm_howto1.mc
dev_magic,/usr1/metacard/cv/.dev_magic.mc
   ? 	dm_audio # 	`       ? ?2i 	KEQUMBWX        audio_option           W 
Helvetica   W 
Helvetica   U 
Helvetica   W 
Helvetica   U 
Helvetica   W 
Helvetica   W 
Helvetica   ? audio_card   	P ?on preOpenCard

  put "You can turn the course audio on or off. " & \
        return & "You can also change the course volume." \
        into field "audio_text"
end preOpenCard



on openCard
  
end openCard
                 2i  ?  ?  ?  ?  ? volume_control   	P ?on preOpenCard
  global @CLOSE, @AUDIO_CONTROL

  put "Volume Control: " into field "volume"

end preOpenCard


on openCard
  
end openCard
                 2i  ?  ?  ?
  ? audio_text   
      ????    ??????     0 `? |            ? *You can turn the course audio on or off.   'You can also change the course volume.   ? 
on_button ?d?x   ?on mouseUp
  global @SOUND
  
  put 1 into @SOUND
  close this stack
end mouseUp

on mouseEnter
  set the hilite of me to true
end mouseEnter


on mouseLeave
  set the hilite of me to false
end mouseLeave
  7 ??????  ??  ??  ??  ??  ??????  ??  ??     ? ? h ,     On         	  ? 	       ? off_button ?d?x   ?on mouseUp
  global @SOUND
  
  put 0 into @SOUND
  close this stack
end mouseUp

on mouseEnter
  set the hilite of me to true
end mouseEnter


on mouseLeave
  set the hilite of me to false
end mouseLeave
  7 ??????  ??  ??  ??  ??  ??????  ??  ??     ? ? ? ,     Off         	  ? 	       ? vol_control ?d?x   ?on mouseUp
  go to next card
end mouseUp

on mouseEnter
  set the hilite of me to true
end mouseEnter


on mouseLeave
  set the hilite of me to false
end mouseLeave
  7 ??????  ??  ??  ??  ??  ??????  ??  ??    < ? ? ,     Volume Control         	  ? 	       ?   ?y    bon mouseUp
  set the playLoudness to the thumbPos of me
  play "./.audio/bells.aiff"
end mouseUp
  ?   ????  ??????    ????  [[WWFF  [[WWFF  [[WWFF  ??????     k t? S?z  ? 0  100       
  ? volume  ?      ????    ??????     d R? /           Volume Control:    ? close_button ?d?x   ?on mouseUp
  close this stack
end mouseUp

on mouseEnter
  set the hilite of me to true
end mouseEnter


on mouseLeave
  set the hilite of me to false
end mouseLeave
  7 ??????  ??  ??  ??  ??  ??uu??  ??  ??    G ? ? ,     Close         	  ? 	      