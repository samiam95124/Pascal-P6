#
# Wrapper series for services module
#
# These wrappers are required for three reasons:
#
# 1. Translate module coining to the final names.
# 2. Perform any parameter translations required.
# 3. Compensate for differences in function returns.
#

    .text

    jmp     1f      # skip stack sequence

    .globl  services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$
    .globl  services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$


services.list$p_vc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    ret

services.list$p_pvc_pr$name$0$pvc$size$8$i$alloc$16$i$attr$24$sx$atexec$atarc$atsys$atdir$atloop$$create$56$i$modify$64$i$access$72$i$backup$80$i$user$88$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$group$120$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$other$152$sx$pmread$pmwrite$pmexec$pmdel$pmvis$pmcopy$pmren$$next$184$p2$:
    movq    %rsi,%rdx           # move **fl to 3rd
    movq    (%rdi),%rsi         # move string len to 2nd
    addq    $8,%rdi             # index string data
    pushq   %rdx                # save **fl
    call    pa_listl            # call C routine
    popq    %rdi                # restore **fl to 1st
    call    cfilelist2pascaline # convert to Pascaline
    ret

#
# Next module in series
#
1:
