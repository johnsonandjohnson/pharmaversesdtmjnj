
ok we want to create a new sdtm data, totally for test purposes..

We need firs to create the PR data DOMAIN for that 

WE JUST ONLY WANT THE VARIABLE THAT WE NEED NOT MORE NO LESS

Here are some specs to help :

Here is everything merged into one plain text block:

```
23 PRELEC    $2        Was this procedure elective?    Was this procedure elective?    Y = Yes
                                                                                      N = No

24 PRFIND    $200      If procedure was diagnostic, provide findings     If procedure was diagnostic, provide findings

4 PRTRT      $200      Therapeutic or Diagnostic Procedure     Therapeutic or Diagnostic Procedure

5 PRINDC     $20       Indication     Indication
                           TRIAL INDICATION 1 = [Acc to Protocol] [ACC TO PROTOCOL]
                           = [Acc to Protocol 2]
                           ADVERSE EVENT = Adverse Event
                           MEDICAL HISTORY = Medical History
                           TRIAL INDICATION = Trial Indication
```


pr DOMAIN similar to cm / dataset !! you can almost copy paste


PRDECODE == pharmaversesdtm::cm$CMDECOD
previntx , just random for now


## Mock Shell We need to be able to produce

LSIMH-JJCS02 

LSIMH-JJCS02: Listing of Procedures; [Analysis Set] Analysis Set (Study jjcs - core) ADSL(FASFL=Y)+ PR+SUPPPR+RELREC 

 

Treatment Group 

ADSL.TRTxxA 

 

Subject ID 

USUBJID 

 

Age (years)   
/Sex/Race 

ADSL.AGE (AGEU)/ SEX/ RACE 

 

Procedure /Surgery   
Planned Before   
Study Entry? 

‘Yes’ when PREVINTX=’BEFORE INTO THE STUDY’; ‘No’ otherwise 

Preferred Term/Reported Term 

PR.PRDECOD 

/PR.PRTRT 

 

Indication 

PR.PRINDC 

 

Start Date/Time  
(Study  Daya) 

PR.PRSTDTC 

(ADSL. TRTSDT) 

 

End Date/Time  
(Study  Daya) 

PR.PRENDTC 

(ADSL. TRTSDT) 

 

Procedure Duration ([Unit]) 

PR.PRDUR 

 

Procedure Elective? 

SUPPPR. PRPLN 

 

Diagnostic Findings 

SUPPPR. PRFIND 

 

Findings Adverse Event? 

’Yes’ when there is a record in RELREC for the PR record where RELID contains text “PROCEDURE FINDINGS”; ‘No’ otherwise 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

a Study day is relative to [the start of [study treatment]].  
 

[Output Identifier] [Program Location] [Date/Time of output] 

 

 

Display Specifications 

Output Identifier 

LSIMH02 

Footnotes 

a Study day is relative to [the start of [study treatment]].  
 

