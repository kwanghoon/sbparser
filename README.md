# sbparser

## Supplementary materials for submission to SAC 2024
 - Commit: #692779c1f9d3c96bf7f5d1f9beecbf04e5bbef3c

### Small Basic grammar and automaton
 - production rules : prod_rules.txt
 - automaton (action and goto tables) : action_table.txt, goto_table.txt
 - states : sb.items

### Syntax completion
 - Learning sets of SmallBasic programs : smallbasic-programs.zip (https://github.com/kwanghoon/SmallBasicDataCollection/tree/main/smallbasic-programs)
 - Test sets of SmallBasic programs : 01_HelloWorld.sb ~ 27_FlickrRepeat.sb (in https://github.com/kwanghoon/MySmallBasic/tree/master/MySmallBasic/Sample)
 - Candidate database : smallbasic-syntax-completion-candidates-results.txt
 - Analysis result on tutorial programs : smallbasic_tutorial_analysis_results.txt

### Prototype editor for supporting the syntax completion
 - https://github.com/kwanghoon/MySmallBasic
 - run java com.coducation.smallbasic.gui.MySmallBasicGUI
 - Enter Ctrl+Space for syntax completion.
 

