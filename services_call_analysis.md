# Services Module Call Analysis

## Direct-Routable Functions (No Conversion Needed)

These functions pass parameters and return values directly with no type
conversion. They can bypass the wrapper entirely using mexternal.

### No Parameters (simple return)

| Function     | Returns  | C Target             |
|-------------|----------|----------------------|
| time        | integer  | services_time        |
| clock       | integer  | services_clock       |
| elapsed     | integer  | services_elapsed     |
| local       | integer  | services_local       |
| latitude    | integer  | services_latitude    |
| longitude   | integer  | services_longitude   |
| altitude    | integer  | services_altitude    |
| country     | integer  | services_country     |
| timezone    | integer  | services_timezone    |
| daysave     | boolean  | services_daysave     |
| time24hour  | boolean  | services_time24hour  |
| language    | integer  | services_language    |
| decimal     | char     | services_decimal     |
| numbersep   | char     | services_numbersep   |
| timeorder   | integer  | services_timeorder   |
| dateorder   | integer  | services_dateorder   |
| datesep     | char     | services_datesep     |
| timesep     | char     | services_timesep     |
| currchr     | char     | services_currchr     |
| optchr      | char     | services_optchr      |
| pthchr      | char     | services_pthchr      |

### Simple Parameter Passthrough

| Function     | Parameters       | C Target             |
|-------------|-----------------|----------------------|
| filchr      | out fc: schar    | services_filchr      |

Total: 22 functions

## Functions Requiring Conversion

### String Conversion (Pascaline fixed-length padded <-> C null-terminated)

| Function       | Conversion Type          | C Target                |
|---------------|--------------------------|-------------------------|
| list          | string unpad + list conv  | services_listl          |
| listp         | pstring + list conv       | services_listl          |
| times         | output string padding     | services_times          |
| timesp        | pstring output            | services_times          |
| dates         | output string padding     | services_dates          |
| datesp        | pstring output            | services_dates          |
| writetime     | string padding            | services_writetime      |
| writetimef    | file + string conversion  | services_writetime      |
| writedate     | string padding            | services_writedate      |
| writedatef    | file + string conversion  | services_writedate      |
| validfile     | input string unpadding    | services_validfilel     |
| validfilep    | pstring unpadding         | services_validfilel     |
| validpath     | input string unpadding    | services_validpathl     |
| validpathp    | pstring unpadding         | services_validpathl     |
| wild          | input string unpadding    | services_wildl          |
| wildp         | pstring unpadding         | services_wildl          |
| getenv        | unpad input + pad output  | services_getenvl        |
| getenvp       | pstring conversion        | services_getenvl        |
| setenv        | input string unpadding    | services_setenvl        |
| setenvps      | mixed pstring/string      | services_setenvl        |
| setenvsp      | mixed string/pstring      | services_setenvl        |
| setenvpp      | pstring conversion        | services_setenvl        |
| remenv        | input string unpadding    | services_remenvl        |
| remenvp       | pstring conversion        | services_remenvl        |
| exec          | input string unpadding    | services_execl          |
| execp         | pstring conversion        | services_execl          |
| exece         | unpad + env list conv     | services_execel         |
| execep        | pstring + env list conv   | services_execel         |
| execw         | input string unpadding    | services_execwl         |
| execwp        | pstring conversion        | services_execwl         |
| execew        | unpad + env list conv     | services_execewl        |
| execewp       | pstring + env list conv   | services_execewl        |
| getcur        | output string padding     | services_getcur         |
| getcurp       | pstring output            | services_getcur         |
| setcur        | input string unpadding    | services_setcurl        |
| setcurp       | pstring conversion        | services_setcurl        |
| brknam        | unpad + pad outputs       | services_brknaml        |
| brknam (ovl2) | unpad + pstring outputs   | services_brknaml        |
| brknam (ovl3) | pstring + pstring outputs | services_brknaml        |
| maknam        | unpad inputs + pad output | services_maknaml        |
| maknam (ovls) | mixed string/pstring (x7) | services_maknaml        |
| fulnam        | string buffer handling    | services_fulnam         |
| fulnamp       | pstring conversion        | services_fulnam         |
| getpgm        | output string padding     | services_getpgm         |
| getpgmp       | pstring output            | services_getpgm         |
| getusr        | output string padding     | services_getusr         |
| getusrp       | pstring output            | services_getusr         |
| countrys      | output string padding     | services_countrys       |
| languages     | output string padding     | services_languages      |
| makpth        | input string unpadding    | services_makpth         |
| makpthp       | pstring conversion        | services_makpth         |
| rempth        | input string unpadding    | services_rempth         |
| rempthp       | pstring conversion        | services_rempth         |

### Set Conversion (Pascaline set <-> C integer)

| Function       | Conversion Type          | C Target                |
|---------------|--------------------------|-------------------------|
| setatr        | set extract + string     | services_setatrl        |
| setatrp       | set extract + pstring    | services_setatrl        |
| resatr        | set extract + string     | services_resatrl        |
| resatrp       | set extract + pstring    | services_resatrl        |
| bakupd        | string unpadding         | services_bakupdl        |
| bakupdp       | pstring conversion       | services_bakupdl        |
| setuper       | set extract + string     | services_setuperl       |
| setuperp      | set extract + pstring    | services_setuperl       |
| resuper       | set extract + string     | services_resuperl       |
| resuperp      | set extract + pstring    | services_resuperl       |
| setgper       | set extract + string     | services_setgperl       |
| setgperp      | set extract + pstring    | services_setgperl       |
| resgper       | set extract + string     | services_resgperl       |
| resgperp      | set extract + pstring    | services_resgperl       |
| setoper       | set extract + string     | services_setoperl       |
| setoperp      | set extract + pstring    | services_setoperl       |
| resoper       | set extract + string     | services_resoperl       |
| resoperp      | set extract + pstring    | services_resoperl       |

### List Conversion (C linked list <-> Pascaline format)

| Function       | Conversion Type          | C Target                |
|---------------|--------------------------|-------------------------|
| allenv        | env list conversion      | services_allenv         |

### Error Handling

| Function       | Conversion Type          | C Target                |
|---------------|--------------------------|-------------------------|
| seterr        | simple passthrough       | services_seterr         |

Total requiring conversion: ~70 wrapper entries

## Notes

1. Bug: wrapper_numbersep calls services_decimal() instead of
   services_numbersep()
2. The string conversion overhead is the dominant cost — eliminating
   Pascaline fixed-string padding would make most wrappers unnecessary
3. Set conversion affects attribute/permission functions only
4. seterr may be direct-routable (needs verification)
