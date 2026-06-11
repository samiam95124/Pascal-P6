# Generated sound trampolines. Do not edit by hand.
    .text

    .globl  sound.aftertouch$p_i_i_x$1$16$i_x$1$128$i_i
    .globl  sound.attack$p_i_i_x$1$16$i_i
    .globl  sound.balance$p_i_i_x$1$16$i_i
    .globl  sound.brightness$p_i_i_x$1$16$i_i
    .globl  sound.celeste$p_i_i_x$1$16$i_i
    .globl  sound.chanwavein$f_i
    .globl  sound.chanwaveout$p_i_i
    .globl  sound.chorus$p_i_i_x$1$16$i_i
    .globl  sound.closesynthin$p_i
    .globl  sound.closesynthout$p_i
    .globl  sound.closewavein$p_i
    .globl  sound.closewaveout$p_i
    .globl  sound.curtimein$f
    .globl  sound.curtimeout$f
    .globl  sound.delsynth$p_i
    .globl  sound.delwave$p_i
    .globl  sound.endwavein$f_i
    .globl  sound.endwaveout$p_i_i
    .globl  sound.fltwavein$f_i
    .globl  sound.fltwaveout$p_i_i
    .globl  sound.getparamsynthin$p_i_vc_vc
    .globl  sound.getparamsynthout$p_i_vc_vc
    .globl  sound.getparamwavein$p_i_vc_vc
    .globl  sound.getparamwaveout$p_i_vc_vc
    .globl  sound.instchange$p_i_i_x$1$16$i_x$1$128$i
    .globl  sound.legato$p_i_i_x$1$16$i_i
    .globl  sound.lenwavein$f_i
    .globl  sound.lenwaveout$p_i_i
    .globl  sound.loadsynth$p_i_vc
    .globl  sound.loadwave$p_i_vc
    .globl  sound.mono$p_i_i_x$1$16$i_i
    .globl  sound.noteoff$p_i_i_x$1$16$i_x$1$128$i_i
    .globl  sound.noteon$p_i_i_x$1$16$i_x$1$128$i_i
    .globl  sound.opensynthin$p_i
    .globl  sound.opensynthout$p_i
    .globl  sound.openwavein$p_i
    .globl  sound.openwaveout$p_i
    .globl  sound.pan$p_i_i_x$1$16$i_i
    .globl  sound.phaser$p_i_i_x$1$16$i_i
    .globl  sound.pitch$p_i_i_x$1$16$i_i
    .globl  sound.pitchrange$p_i_i_x$1$16$i_i
    .globl  sound.playsynth$p_i_i_i
    .globl  sound.playwave$p_i_i_i
    .globl  sound.poly$p_i_i_x$1$16$i
    .globl  sound.portamento$p_i_i_x$1$16$i_i
    .globl  sound.porttime$p_i_i_x$1$16$i_i
    .globl  sound.pressure$p_i_i_x$1$16$i_i
    .globl  sound.ratewavein$f_i
    .globl  sound.ratewaveout$p_i_i
    .globl  sound.rdwave$f_i_vx$0$255$i
    .globl  sound.release$p_i_i_x$1$16$i_i
    .globl  sound.reverb$p_i_i_x$1$16$i_i
    .globl  sound.setparamsynthin$f_i_vc_vc
    .globl  sound.setparamsynthout$f_i_vc_vc
    .globl  sound.setparamwavein$f_i_vc_vc
    .globl  sound.setparamwaveout$f_i_vc_vc
    .globl  sound.sgnwavein$f_i
    .globl  sound.sgnwaveout$p_i_i
    .globl  sound.starttimein$p
    .globl  sound.starttimeout$p
    .globl  sound.stoptimein$p
    .globl  sound.stoptimeout$p
    .globl  sound.synthin$f
    .globl  sound.synthinname$p_i_vc
    .globl  sound.synthout$f
    .globl  sound.synthoutname$p_i_vc
    .globl  sound.timbre$p_i_i_x$1$16$i_i
    .globl  sound.tremulo$p_i_i_x$1$16$i_i
    .globl  sound.vibrato$p_i_i_x$1$16$i_i
    .globl  sound.volsynthchan$p_i_i_x$1$16$i_i
    .globl  sound.volwave$p_i_i_i
    .globl  sound.waitsynth$p_i
    .globl  sound.waitwave$p_i
    .globl  sound.wavein$f
    .globl  sound.waveinname$p_i_vc
    .globl  sound.waveout$f
    .globl  sound.waveoutname$p_i_vc
    .globl  sound.wrwave$p_i_vx$0$255$i

sound.aftertouch$p_i_i_x$1$16$i_x$1$128$i_i:
    jmp     wrapper_aftertouch

sound.attack$p_i_i_x$1$16$i_i:
    jmp     wrapper_attack

sound.balance$p_i_i_x$1$16$i_i:
    jmp     wrapper_balance

sound.brightness$p_i_i_x$1$16$i_i:
    jmp     wrapper_brightness

sound.celeste$p_i_i_x$1$16$i_i:
    jmp     wrapper_celeste

sound.chanwavein$f_i:
    jmp     wrapper_chanwavein

sound.chanwaveout$p_i_i:
    jmp     wrapper_chanwaveout

sound.chorus$p_i_i_x$1$16$i_i:
    jmp     wrapper_chorus

sound.closesynthin$p_i:
    jmp     wrapper_closesynthin

sound.closesynthout$p_i:
    jmp     wrapper_closesynthout

sound.closewavein$p_i:
    jmp     wrapper_closewavein

sound.closewaveout$p_i:
    jmp     wrapper_closewaveout

sound.curtimein$f:
    jmp     wrapper_curtimein

sound.curtimeout$f:
    jmp     wrapper_curtimeout

sound.delsynth$p_i:
    jmp     wrapper_delsynth

sound.delwave$p_i:
    jmp     wrapper_delwave

sound.endwavein$f_i:
    jmp     wrapper_endwavein

sound.endwaveout$p_i_i:
    jmp     wrapper_endwaveout

sound.fltwavein$f_i:
    jmp     wrapper_fltwavein

sound.fltwaveout$p_i_i:
    jmp     wrapper_fltwaveout

sound.getparamsynthin$p_i_vc_vc:
    jmp     wrapper_getparamsynthin

sound.getparamsynthout$p_i_vc_vc:
    jmp     wrapper_getparamsynthout

sound.getparamwavein$p_i_vc_vc:
    jmp     wrapper_getparamwavein

sound.getparamwaveout$p_i_vc_vc:
    jmp     wrapper_getparamwaveout

sound.instchange$p_i_i_x$1$16$i_x$1$128$i:
    jmp     wrapper_instchange

sound.legato$p_i_i_x$1$16$i_i:
    jmp     wrapper_legato

sound.lenwavein$f_i:
    jmp     wrapper_lenwavein

sound.lenwaveout$p_i_i:
    jmp     wrapper_lenwaveout

sound.loadsynth$p_i_vc:
    jmp     wrapper_loadsynth

sound.loadwave$p_i_vc:
    jmp     wrapper_loadwave

sound.mono$p_i_i_x$1$16$i_i:
    jmp     wrapper_mono

sound.noteoff$p_i_i_x$1$16$i_x$1$128$i_i:
    jmp     wrapper_noteoff

sound.noteon$p_i_i_x$1$16$i_x$1$128$i_i:
    jmp     wrapper_noteon

sound.opensynthin$p_i:
    jmp     wrapper_opensynthin

sound.opensynthout$p_i:
    jmp     wrapper_opensynthout

sound.openwavein$p_i:
    jmp     wrapper_openwavein

sound.openwaveout$p_i:
    jmp     wrapper_openwaveout

sound.pan$p_i_i_x$1$16$i_i:
    jmp     wrapper_pan

sound.phaser$p_i_i_x$1$16$i_i:
    jmp     wrapper_phaser

sound.pitch$p_i_i_x$1$16$i_i:
    jmp     wrapper_pitch

sound.pitchrange$p_i_i_x$1$16$i_i:
    jmp     wrapper_pitchrange

sound.playsynth$p_i_i_i:
    jmp     wrapper_playsynth

sound.playwave$p_i_i_i:
    jmp     wrapper_playwave

sound.poly$p_i_i_x$1$16$i:
    jmp     wrapper_poly

sound.portamento$p_i_i_x$1$16$i_i:
    jmp     wrapper_portamento

sound.porttime$p_i_i_x$1$16$i_i:
    jmp     wrapper_porttime

sound.pressure$p_i_i_x$1$16$i_i:
    jmp     wrapper_pressure

sound.ratewavein$f_i:
    jmp     wrapper_ratewavein

sound.ratewaveout$p_i_i:
    jmp     wrapper_ratewaveout

sound.rdwave$f_i_vx$0$255$i:
    jmp     wrapper_rdwave

sound.release$p_i_i_x$1$16$i_i:
    jmp     wrapper_release

sound.reverb$p_i_i_x$1$16$i_i:
    jmp     wrapper_reverb

sound.setparamsynthin$f_i_vc_vc:
    jmp     wrapper_setparamsynthin

sound.setparamsynthout$f_i_vc_vc:
    jmp     wrapper_setparamsynthout

sound.setparamwavein$f_i_vc_vc:
    jmp     wrapper_setparamwavein

sound.setparamwaveout$f_i_vc_vc:
    jmp     wrapper_setparamwaveout

sound.sgnwavein$f_i:
    jmp     wrapper_sgnwavein

sound.sgnwaveout$p_i_i:
    jmp     wrapper_sgnwaveout

sound.starttimein$p:
    jmp     wrapper_starttimein

sound.starttimeout$p:
    jmp     wrapper_starttimeout

sound.stoptimein$p:
    jmp     wrapper_stoptimein

sound.stoptimeout$p:
    jmp     wrapper_stoptimeout

sound.synthin$f:
    jmp     wrapper_synthin

sound.synthinname$p_i_vc:
    jmp     wrapper_synthinname

sound.synthout$f:
    jmp     wrapper_synthout

sound.synthoutname$p_i_vc:
    jmp     wrapper_synthoutname

sound.timbre$p_i_i_x$1$16$i_i:
    jmp     wrapper_timbre

sound.tremulo$p_i_i_x$1$16$i_i:
    jmp     wrapper_tremulo

sound.vibrato$p_i_i_x$1$16$i_i:
    jmp     wrapper_vibrato

sound.volsynthchan$p_i_i_x$1$16$i_i:
    jmp     wrapper_volsynthchan

sound.volwave$p_i_i_i:
    jmp     wrapper_volwave

sound.waitsynth$p_i:
    jmp     wrapper_waitsynth

sound.waitwave$p_i:
    jmp     wrapper_waitwave

sound.wavein$f:
    jmp     wrapper_wavein

sound.waveinname$p_i_vc:
    jmp     wrapper_waveinname

sound.waveout$f:
    jmp     wrapper_waveout

sound.waveoutname$p_i_vc:
    jmp     wrapper_waveoutname

sound.wrwave$p_i_vx$0$255$i:
    jmp     wrapper_wrwave

