#
# Wrapper series for terminal module
#
# With amd64sysv, the Pascaline calling convention matches the AMD64 System V
# ABI. The only task remaining for this wrapper is name coining: translating
# from the Pascaline mangled symbol format
#
#     <module name>.<routine>$<type signature>
#
# to the C name of the wrapper function. Each entry is a simple jmp (tail call).
#

    .globl  terminal.auto$p_b
    .globl  terminal.auto$p_fc_b
    .globl  terminal.autohold$p_b
    .globl  terminal.back$p
    .globl  terminal.back$p_fc
    .globl  terminal.bcolor$p_fc_x$black$white$red$green$blue$cyan$yellow$magenta$
    .globl  terminal.bcolor$p_x$black$white$red$green$blue$cyan$yellow$magenta$
    .globl  terminal.bcolorc$p_fc_i_i_i
    .globl  terminal.bcolorc$p_i_i_i
    .globl  terminal.blink$p_b
    .globl  terminal.blink$p_fc_b
    .globl  terminal.bold$p_b
    .globl  terminal.bold$p_fc_b
    .globl  terminal.buffer$p_b
    .globl  terminal.buffer$p_fc_b
    .globl  terminal.clrtab$p
    .globl  terminal.clrtab$p_fc
    .globl  terminal.curbnd$f
    .globl  terminal.curbnd$f_fc
    .globl  terminal.cursor$p_fc_i_i
    .globl  terminal.cursor$p_i_i
    .globl  terminal.curvis$p_b
    .globl  terminal.curvis$p_fc_b
    .globl  terminal.curx$f
    .globl  terminal.curx$f_fc
    .globl  terminal.cury$f
    .globl  terminal.cury$f_fc
    .globl  terminal.deinitlock$p_i
    .globl  terminal.deinitsig$p_i
    .globl  terminal.del$p
    .globl  terminal.del$p_fc
    .globl  terminal.democall$p_q$$
    .globl  terminal.demoevent$p_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$
    .globl  terminal.down$p
    .globl  terminal.down$p_fc
    .globl  terminal.event$p_fc_r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$
    .globl  terminal.event$p_r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$
    .globl  terminal.eventover$p_x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$_i
    .globl  terminal.eventsover$p_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$_i
    .globl  terminal.fcolor$p_fc_x$black$white$red$green$blue$cyan$yellow$magenta$
    .globl  terminal.fcolor$p_x$black$white$red$green$blue$cyan$yellow$magenta$
    .globl  terminal.fcolorc$p_fc_i_i_i
    .globl  terminal.fcolorc$p_i_i_i
    .globl  terminal.focus$p
    .globl  terminal.focus$p_fc
    .globl  terminal.frame$p_b
    .globl  terminal.frame$p_fc_b
    .globl  terminal.frametimer$p_b
    .globl  terminal.frametimer$p_fc_b
    .globl  terminal.front$p
    .globl  terminal.front$p_fc
    .globl  terminal.funkey$f
    .globl  terminal.funkey$f_fc
    .globl  terminal.getsiz$p_fc_i_i
    .globl  terminal.getsiz$p_i_i
    .globl  terminal.getwinid$f
    .globl  terminal.home$p
    .globl  terminal.home$p_fc
    .globl  terminal.initlock$f
    .globl  terminal.initsig$f
    .globl  terminal.italic$p_b
    .globl  terminal.italic$p_fc_b
    .globl  terminal.joyaxis$f_fc_i
    .globl  terminal.joyaxis$f_i
    .globl  terminal.joybutton$f_fc_i
    .globl  terminal.joybutton$f_i
    .globl  terminal.joystick$f
    .globl  terminal.joystick$f_fc
    .globl  terminal.killtimer$p_fc_i
    .globl  terminal.killtimer$p_i
    .globl  terminal.left$p
    .globl  terminal.left$p_fc
    .globl  terminal.lock$p_i
    .globl  terminal.maxx$f
    .globl  terminal.maxx$f_fc
    .globl  terminal.maxy$f
    .globl  terminal.maxy$f_fc
    .globl  terminal.mouse$f
    .globl  terminal.mouse$f_fc
    .globl  terminal.mousebutton$f_fc_i
    .globl  terminal.mousebutton$f_i
    .globl  terminal.newthread$f_q$$
    .globl  terminal.restab$p_fc_i
    .globl  terminal.restab$p_i
    .globl  terminal.reverse$p_b
    .globl  terminal.reverse$p_fc_b
    .globl  terminal.right$p
    .globl  terminal.right$p_fc
    .globl  terminal.scncen$p_fc_i_i
    .globl  terminal.scncen$p_i_i
    .globl  terminal.scnsiz$p_fc_i_i
    .globl  terminal.scnsiz$p_i_i
    .globl  terminal.scroll$p_fc_i_i
    .globl  terminal.scroll$p_i_i
    .globl  terminal.select$p_fc_i_i
    .globl  terminal.select$p_i_i
    .globl  terminal.sendsig$p_i
    .globl  terminal.sendsigone$p_i
    .globl  terminal.setpos$p_fc_i_i
    .globl  terminal.setpos$p_i_i
    .globl  terminal.setsiz$p_fc_i_i
    .globl  terminal.setsiz$p_i_i
    .globl  terminal.settab$p_fc_i
    .globl  terminal.settab$p_i
    .globl  terminal.sizable$p_b
    .globl  terminal.sizable$p_fc_b
    .globl  terminal.sizbuf$p_fc_i_i
    .globl  terminal.sizbuf$p_i_i
    .globl  terminal.standout$p_b
    .globl  terminal.standout$p_fc_b
    .globl  terminal.strikeout$p_b
    .globl  terminal.strikeout$p_fc_b
    .globl  terminal.subscript$p_b
    .globl  terminal.subscript$p_fc_b
    .globl  terminal.superscript$p_b
    .globl  terminal.superscript$p_fc_b
    .globl  terminal.sysbar$p_b
    .globl  terminal.sysbar$p_fc_b
    .globl  terminal.timer$p_fc_i_i_b
    .globl  terminal.timer$p_i_i_b
    .globl  terminal.title$p_fc_vc
    .globl  terminal.title$p_vc
    .globl  terminal.underline$p_b
    .globl  terminal.underline$p_fc_b
    .globl  terminal.unlock$p_i
    .globl  terminal.up$p
    .globl  terminal.up$p_fc
    .globl  terminal.waitsig$p_i_i
    .globl  terminal.wrtstr$p_fc_vc
    .globl  terminal.wrtstr$p_vc

    .text

    jmp     1f      # skip wrapper sequence

terminal.auto$p_b:
    jmp     wrapper_auto

terminal.auto$p_fc_b:
    jmp     wrapper_autof

terminal.autohold$p_b:
    jmp     wrapper_autohold

terminal.back$p:
    jmp     wrapper_back

terminal.back$p_fc:
    jmp     wrapper_backf

terminal.bcolor$p_fc_x$black$white$red$green$blue$cyan$yellow$magenta$:
    jmp     wrapper_bcolorf

terminal.bcolor$p_x$black$white$red$green$blue$cyan$yellow$magenta$:
    jmp     wrapper_bcolor

terminal.bcolorc$p_fc_i_i_i:
    jmp     wrapper_bcolorcf

terminal.bcolorc$p_i_i_i:
    jmp     wrapper_bcolorc

terminal.blink$p_b:
    jmp     wrapper_blink

terminal.blink$p_fc_b:
    jmp     wrapper_blinkf

terminal.bold$p_b:
    jmp     wrapper_bold

terminal.bold$p_fc_b:
    jmp     wrapper_boldf

terminal.buffer$p_b:
    jmp     wrapper_buffer

terminal.buffer$p_fc_b:
    jmp     wrapper_bufferf

terminal.clrtab$p:
    jmp     wrapper_clrtab

terminal.clrtab$p_fc:
    jmp     wrapper_clrtabf

terminal.curbnd$f:
    jmp     wrapper_curbnd

terminal.curbnd$f_fc:
    jmp     wrapper_curbndf

terminal.cursor$p_fc_i_i:
    jmp     wrapper_cursorf

terminal.cursor$p_i_i:
    jmp     wrapper_cursor

terminal.curvis$p_b:
    jmp     wrapper_curvis

terminal.curvis$p_fc_b:
    jmp     wrapper_curvisf

terminal.curx$f:
    jmp     wrapper_curx

terminal.curx$f_fc:
    jmp     wrapper_curxf

terminal.cury$f:
    jmp     wrapper_cury

terminal.cury$f_fc:
    jmp     wrapper_curyf

terminal.deinitlock$p_i:
    jmp     wrapper_deinitlock

terminal.deinitsig$p_i:
    jmp     wrapper_deinitsig

terminal.del$p:
    jmp     wrapper_del

terminal.del$p_fc:
    jmp     wrapper_delf

terminal.democall$p_q$$:
    jmp     wrapper_democall

terminal.demoevent$p_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$:
    jmp     wrapper_demoevent

terminal.down$p:
    jmp     wrapper_down

terminal.down$p_fc:
    jmp     wrapper_downf

terminal.event$p_fc_r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$:
    jmp     wrapper_eventf

terminal.event$p_r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$:
    jmp     wrapper_event

terminal.eventover$p_x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$_i:
    jmp     wrapper_eventover

terminal.eventsover$p_q$r$winid$0$i$handled$8$b$etype$9$x$etchar$etup$etdown$etleft$etright$etleftw$etrightw$ethome$ethomes$ethomel$etend$etends$etendl$etscrl$etscrr$etscru$etscrd$etpagd$etpagu$ettab$etenter$etinsert$etinsertl$etinsertt$etdel$etdell$etdelcf$etdelcb$etcopy$etcopyl$etcan$etstop$etcont$etprint$etprintb$etprints$etfun$etmenu$etmouba$etmoubd$etmoumov$ettim$etjoyba$etjoybd$etjoymov$etresize$etfocus$etnofocus$ethover$etnohover$etterm$etframe$etredraw$etmin$etmax$etnorm$etmenus$$55$$54$$53$$52$$51$$50$$49$$48$$47$$46$$37$$35$$34$$33$$32$$31$$30$$29$$28$$27$$26$$25$$24$$23$$22$$21$$20$$19$$18$$17$$16$$15$$14$$13$$12$$11$$10$$9$$8$$7$$6$$5$$4$$3$$2$$1$$56$menuid$12$i$45$rszx$20$i$rszy$12$i$36$fkey$12$i$44$mjoyn$60$i$joypx$52$i$joypy$44$i$joypz$36$i$joyp4$28$i$joyp5$20$i$joyp6$12$i$43$djoyn$20$i$djoybn$12$i$42$ajoyn$20$i$ajoybn$12$i$39$dmoun$20$i$dmoubn$12$i$38$amoun$20$i$amoubn$12$i$40$mmoun$28$i$moupx$20$i$moupy$12$i$41$timnum$12$i$0$echar$12$c$$$$_i:
    jmp     wrapper_eventsover

terminal.fcolor$p_fc_x$black$white$red$green$blue$cyan$yellow$magenta$:
    jmp     wrapper_fcolorf

terminal.fcolor$p_x$black$white$red$green$blue$cyan$yellow$magenta$:
    jmp     wrapper_fcolor

terminal.fcolorc$p_fc_i_i_i:
    jmp     wrapper_fcolorcf

terminal.fcolorc$p_i_i_i:
    jmp     wrapper_fcolorc

terminal.focus$p:
    jmp     wrapper_focus

terminal.focus$p_fc:
    jmp     wrapper_focusf

terminal.frame$p_b:
    jmp     wrapper_frame

terminal.frame$p_fc_b:
    jmp     wrapper_framef

terminal.frametimer$p_b:
    jmp     wrapper_frametimer

terminal.frametimer$p_fc_b:
    jmp     wrapper_frametimerf

terminal.front$p:
    jmp     wrapper_front

terminal.front$p_fc:
    jmp     wrapper_frontf

terminal.funkey$f:
    jmp     wrapper_funkey

terminal.funkey$f_fc:
    jmp     wrapper_funkeyf

terminal.getsiz$p_fc_i_i:
    jmp     wrapper_getsizf

terminal.getsiz$p_i_i:
    jmp     wrapper_getsiz

terminal.getwinid$f:
    jmp     wrapper_getwinid

terminal.home$p:
    jmp     wrapper_home

terminal.home$p_fc:
    jmp     wrapper_homef

terminal.initlock$f:
    jmp     wrapper_initlock

terminal.initsig$f:
    jmp     wrapper_initsig

terminal.italic$p_b:
    jmp     wrapper_italic

terminal.italic$p_fc_b:
    jmp     wrapper_italicf

terminal.joyaxis$f_fc_i:
    jmp     wrapper_joyaxisf

terminal.joyaxis$f_i:
    jmp     wrapper_joyaxis

terminal.joybutton$f_fc_i:
    jmp     wrapper_joybuttonf

terminal.joybutton$f_i:
    jmp     wrapper_joybutton

terminal.joystick$f:
    jmp     wrapper_joystick

terminal.joystick$f_fc:
    jmp     wrapper_joystickf

terminal.killtimer$p_fc_i:
    jmp     wrapper_killtimerf

terminal.killtimer$p_i:
    jmp     wrapper_killtimer

terminal.left$p:
    jmp     wrapper_left

terminal.left$p_fc:
    jmp     wrapper_leftf

terminal.lock$p_i:
    jmp     wrapper_lock

terminal.maxx$f:
    jmp     wrapper_maxx

terminal.maxx$f_fc:
    jmp     wrapper_maxxf

terminal.maxy$f:
    jmp     wrapper_maxy

terminal.maxy$f_fc:
    jmp     wrapper_maxyf

terminal.mouse$f:
    jmp     wrapper_mouse

terminal.mouse$f_fc:
    jmp     wrapper_mousef

terminal.mousebutton$f_fc_i:
    jmp     wrapper_mousebuttonf

terminal.mousebutton$f_i:
    jmp     wrapper_mousebutton

terminal.newthread$f_q$$:
    jmp     wrapper_newthread

terminal.restab$p_fc_i:
    jmp     wrapper_restabf

terminal.restab$p_i:
    jmp     wrapper_restab

terminal.reverse$p_b:
    jmp     wrapper_reverse

terminal.reverse$p_fc_b:
    jmp     wrapper_reversef

terminal.right$p:
    jmp     wrapper_right

terminal.right$p_fc:
    jmp     wrapper_rightf

terminal.scncen$p_fc_i_i:
    jmp     wrapper_scncenf

terminal.scncen$p_i_i:
    jmp     wrapper_scncen

terminal.scnsiz$p_fc_i_i:
    jmp     wrapper_scnsizf

terminal.scnsiz$p_i_i:
    jmp     wrapper_scnsiz

terminal.scroll$p_fc_i_i:
    jmp     wrapper_scrollf

terminal.scroll$p_i_i:
    jmp     wrapper_scroll

terminal.select$p_fc_i_i:
    jmp     wrapper_selectf

terminal.select$p_i_i:
    jmp     wrapper_select

terminal.sendsig$p_i:
    jmp     wrapper_sendsig

terminal.sendsigone$p_i:
    jmp     wrapper_sendsigone

terminal.setpos$p_fc_i_i:
    jmp     wrapper_setposf

terminal.setpos$p_i_i:
    jmp     wrapper_setpos

terminal.setsiz$p_fc_i_i:
    jmp     wrapper_setsizf

terminal.setsiz$p_i_i:
    jmp     wrapper_setsiz

terminal.settab$p_fc_i:
    jmp     wrapper_settabf

terminal.settab$p_i:
    jmp     wrapper_settab

terminal.sizable$p_b:
    jmp     wrapper_sizable

terminal.sizable$p_fc_b:
    jmp     wrapper_sizablef

terminal.sizbuf$p_fc_i_i:
    jmp     wrapper_sizbuff

terminal.sizbuf$p_i_i:
    jmp     wrapper_sizbuf

terminal.standout$p_b:
    jmp     wrapper_standout

terminal.standout$p_fc_b:
    jmp     wrapper_standoutf

terminal.strikeout$p_b:
    jmp     wrapper_strikeout

terminal.strikeout$p_fc_b:
    jmp     wrapper_strikeoutf

terminal.subscript$p_b:
    jmp     wrapper_subscript

terminal.subscript$p_fc_b:
    jmp     wrapper_subscriptf

terminal.superscript$p_b:
    jmp     wrapper_superscript

terminal.superscript$p_fc_b:
    jmp     wrapper_superscriptf

terminal.sysbar$p_b:
    jmp     wrapper_sysbar

terminal.sysbar$p_fc_b:
    jmp     wrapper_sysbarf

terminal.timer$p_fc_i_i_b:
    jmp     wrapper_timerf

terminal.timer$p_i_i_b:
    jmp     wrapper_timer

terminal.title$p_fc_vc:
    jmp     wrapper_titlef

terminal.title$p_vc:
    jmp     wrapper_title

terminal.underline$p_b:
    jmp     wrapper_underline

terminal.underline$p_fc_b:
    jmp     wrapper_underlinef

terminal.unlock$p_i:
    jmp     wrapper_unlock

terminal.up$p:
    jmp     wrapper_up

terminal.up$p_fc:
    jmp     wrapper_upf

terminal.waitsig$p_i_i:
    jmp     wrapper_waitsig

terminal.wrtstr$p_fc_vc:
    jmp     wrapper_wrtstrf

terminal.wrtstr$p_vc:
    jmp     wrapper_wrtstr

#
# Next module in series
#
1:
