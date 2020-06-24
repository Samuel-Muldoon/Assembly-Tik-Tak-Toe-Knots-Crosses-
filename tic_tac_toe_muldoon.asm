COMMENT /* TIC_TAK_TOE_MULDOON.asm

    PURPOSE:

       This program allows a user to play a game of tic-tak-toe
       against the computer or watch the computer play a game of
       tic-tak-toe against itself.

    CHANGE LOG:

           Date               Engineer       Description of Changes
        ==========           ==========      ======================
     2016    May    10     Samuel Muldoon    Original Code.

     2016   month   dd     your name here    description     
                                             more description 
*/

INCLUDE Irvine32.inc;

.data?; // begin uninitialized data segment

.data

    ;// memory for statistics
        stats  DWORD 0; // game count
               DWORD 0; // game_count_won
               DWORD 0; // game_count_tie
               DWORD 0; // game_count_lost
               DWORD 0; // game_count_pvc
               DWORD 0; // game_count_pvc_user_win
               DWORD 0; // game_count_pvc_comp_win
               DWORD 0; // game_count_pvc_tie 
               DWORD 0; // game_count_cvc 
               DWORD 0; // game_count_cvc_comp1_win
               DWORD 0; // game_count_cvc_comp2_win
               DWORD 0; // game_count_cvc_tie 

    ;// memory for main menu
        menu_main_title        BYTE "TIC-TAC-TOE MAIN MENU", 0;
        menu_main_opt_PvAI     BYTE "Player vs computer", 0;
        menu_main_opt_AIvAI    BYTE "Computer vs computer", 0;
        menu_main_opt_stats    BYTE "Display stats", 0;
        menu_main_opt_exit     BYTE "Exit", 0;

        main_menu              DWORD OFFSET menu_main_title;
                               DWORD OFFSET menu_main_opt_PvAI;
                               DWORD OFFSET menu_main_opt_AIvAI;
                               DWORD OFFSET menu_main_opt_stats;
                               DWORD OFFSET menu_main_opt_exit;
                               DWORD 0;

        main_menu_option_num_of_quit = (($ - main_menu) / (TYPE main_menu)) - 2;

    
.code; // begin code segment

    DISPLAY_STATS_SUBMENU PROTO, offset_of_stats_all:DWORD

    DISPLAY_STATS_ALL PROTO, offset_of_stats_all:DWORD

    UPDATE_STATS PROTO, offset_of_stats_all:DWORD, pvc_or_cvc:DWORD, who_won:DWORD

    PLAY_A_ROUND_OF_COMP_VS_COMP PROTO;

    PLAY_A_ROUND_OF_COMP_VS_USER PROTO;

    ASK_USER_IF_THEY_WANT_TO_GO_AGAIN PROTO;

    PRINT_BOARD PROTO, tic_tac_board:DWORD;

    PRINT_BOARD_WITH_SOME_HIGHLIGHTED PROTO, tic_tac_board:DWORD, which_to_highlight:DWORD;

    MAKE_MOVE PROTO, tic_tac_board : DWORD, row : DWORD, col : DWORD, naught_or_cross : DWORD;

    IS_SQUARE_PART_OF_3_IN_A_ROW PROTO, tic_tac_board:DWORD, row:DWORD, col:DWORD;

    GET_USER_MOVE PROTO, tic_tac_board4:DWORD, symbol:DWORD;

    GET_COMPUTER_MOVE PROTO, tic_tac_board4:DWORD;

    RAND_SELECT_AVAILABLE_SPOT PROTO, tic_tac_board3:DWORD;

    COUNT_AVAILABLE_SPOTS PROTO, tic_tac_board2:DWORD;

    IS_SPOT_AVAILABLE PROTO, tic_tac_board:DWORD, row:DWORD, col:DWORD;

    USER_INT_RANGE_LOOPED PROTO, min:DWORD, max:DWORD, UIBL_prompt:DWORD;

    USER_INT_BOUNDED PROTO, min:DWORD, max:DWORD, UIB_prompt:DWORD;

    USER_INT PROTO, string:DWORD;

    IS_NUMERIC_STRING PROTO, string:DWORD;

    TRIM PROTO, string:DWORD;

    IS_WHITE PROTO, character:DWORD; 

    CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT PROTO, linear_index:DWORD;

    CONVERT_ROW_COL_TO_LINEAR_INDEX PROTO, row:DWORD, col:DWORD;

    RANDOM_CARRY_FLAG PROTO;

    WAIT_MILLISECONDS PROTO, time:DWORD;

    PRINT_MENU_AND_GET_SELECTION PROTO, menu_opts:DWORD;

    STR_LEN PROTO, string:DWORD; 

    PRINT_HEADER PROTO, offset_of_str:DWORD, quantity:DWORD;

    REPEAT_CHAR PROTO, character:DWORD, quantity : DWORD;

COMMENT $/*        
  __  __      _      ___   _   _ 
 |  \/  |    / \    |_ _| | \ | |
 | |\/| |   / _ \    | |  |  \| |
 | |  | |  / ___ \   | |  | |\  |
 |_|  |_| /_/   \_\ |___| |_| \_| 
*/$                                

main PROC; //create main procedure

    ;// Seed the random number generator
        CALL RANDOMIZE;

    menu_loop_start:

        ;// Print_main_menu_and_get_selection:
            MOV esi, OFFSET main_menu;
            ;// user selection ecx <-
                INVOKE PRINT_MENU_AND_GET_SELECTION, esi;

        ;// Process menu selection
            ;// If user chose to quit, then quit
                CMP ecx, main_menu_option_num_of_quit;
                JNC menu_loop_end;
            ;// Otherwise, do task corresponding to user's menu selection
                ;// clear screen
                    CALL CLRSCR;

                ;// 
                    CMP ecx, 1; //  1: Player vs computer
                    JNZ compare_menu_select_to_2;

                        MOV eax, ecx;

                        play_p_vs_c:
   
                            INVOKE PLAY_A_ROUND_OF_COMP_VS_USER;
                            ;// ECX: 1 if user won
                            ;//      2 if user tied with computer
                            ;//      3 if computer won.
                       
                            INVOKE UPDATE_STATS, OFFSET stats, eax       , ecx;
                        ;// INVOKE UPDATE_STATS, OFFSET stats, pvc_or_cvc, who_won;

                            INVOKE ASK_USER_IF_THEY_WANT_TO_GO_AGAIN;
                            JZ finished_performing_actions_based_on_menu_selection;

                        JMP play_p_vs_c;

                    compare_menu_select_to_2:
                    CMP ecx, 2; //  2: Computer vs computer
                    JNZ menu_select_was_3;

                        MOV eax, ecx;

                        play_c_vs_c:
  
                            INVOKE PLAY_A_ROUND_OF_COMP_VS_COMP;
                            ;// ECX: 1 if computer 1 won
                            ;//      2 if computer tied with itself
                            ;//      3 if computer 2 won.

                            INVOKE UPDATE_STATS, OFFSET stats, eax       , ecx;
                        ;// INVOKE UPDATE_STATS, OFFSET stats, pvc_or_cvc, who_won;

                            INVOKE ASK_USER_IF_THEY_WANT_TO_GO_AGAIN;
                            JZ finished_performing_actions_based_on_menu_selection;

                        JMP play_c_vs_c;

                    menu_select_was_3: ;// Display stats

                        INVOKE DISPLAY_STATS_SUBMENU, OFFSET stats;

            finished_performing_actions_based_on_menu_selection:       
            
        ;// clear screen
            CALL CLRSCR;

        ;//
            JMP menu_loop_start;

    menu_loop_end:

    ;// Quit:
        INVOKE DISPLAY_STATS_ALL, OFFSET stats;
        CALL CRLF; // print line-break
        exit;

main ENDP; // end definition of main

COMMENT /************************************************************
        DISPLAY_STATS_SUBMENU

        Description:           Displays a menu asking the user for
                               which of various statistics they
                               want displayed. Statistics include
                               such items as   the total number of
                               matches played, the total number of
                               matches won by the user in user versus
                               computer mode, etc...

        Preconditions:         STACK: Is not full. Yopmost DWORD of
                                     the stack contains
                                     offset_of_stats_all, where:

      [offset_of_stats_all + 0 *4] == game count
      [offset_of_stats_all + 1 *4] == game_count_won
      [offset_of_stats_all + 2 *4] == game_count_tie
      [offset_of_stats_all + 3 *4] == game_count_lost
      [offset_of_stats_all + 4 *4] == game_count user versus computer
      [offset_of_stats_all + 5 *4] == game_count user versus computer_user_win
      [offset_of_stats_all + 6 *4] == game_count user versus computer_comp_win
      [offset_of_stats_all + 7 *4] == game_count user versus computer_tie 
      [offset_of_stats_all + 8 *4] == game_count computer v computer 
      [offset_of_stats_all + 9 *4] == game_count computer v computer_comp1_win
      [offset_of_stats_all + 10*4] == game_count computer v computer_comp2_win
      [offset_of_stats_all + 11*4] == game_count computer v computer_tie                               

        Postconditions:         STACK: Input to procedure (1 DWORD) has
                                       been removed from top of the stack.

                              CONSOLE: menu and user-chosen statistics
                                       were displayed to the console.    

*********************************************************************/
DISPLAY_STATS_SUBMENU PROC, offset_of_stats_all:DWORD
    .data
        menu_stats_title                         BYTE "STATISTICS MENU                                          ", 0;
        menu_stats_opt_game_count                BYTE "Total number of matches played                           ", 0;              
        menu_stats_opt_game_count_won            BYTE "Total number of matches won                              ", 0;          
        menu_stats_opt_game_count_tie            BYTE "Total number of matches tied                             ", 0;        
        menu_stats_opt_game_count_lost           BYTE "Total number of matches lost                             ", 0;        
        menu_stats_opt_game_count_pvc            BYTE "Total number of matches    played    in user vs comp mode", 0;           
        menu_stats_opt_game_count_pvc_user_win   BYTE "Total number of matches won by user  in user vs comp mode", 0;  
        menu_stats_opt_game_count_pvc_comp_win   BYTE "Total number of matches won by comp  in user vs comp mode", 0;  
        menu_stats_opt_game_count_pvc_tie        BYTE "Total number of matches     tied     in user vs comp mode", 0;    
        menu_stats_opt_game_count_cvc            BYTE "Total number of matches    played    in comp vs comp mode", 0; 
        menu_stats_opt_game_count_cvc_comp1_win  BYTE "Total number of matches won by comp1 in comp vs comp mode", 0; 
        menu_stats_opt_game_count_cvc_comp2_win  BYTE "Total number of matches won by comp2 in comp vs comp mode", 0;
        menu_stats_opt_game_count_cvc_tie        BYTE "Total number of matches     tied     in comp vs comp mode", 0;
        menu_stats_opt_all                       BYTE "All Stats                                                ", 0;     
        menu_stats_opt_exit                      BYTE "Return to main menu                                      ", 0;

    .code

    ;// Reserve memory on the stack for local variables

        menu_stats_numel = 16;

        menu_stats_nbytes = menu_stats_numel * (TYPE DWORD);

        menu_stats_option_num_of_quit = menu_stats_numel - 2;

        total_nbytes = menu_stats_nbytes;
        
        ;// want to end on a double word boundary
        ;// dword_count = CEILING (total_nbytes / 4);
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        dword_count = 1 + ((total_nbytes - 1) / 4);
        SUB esp, 4*dword_count;

    ;// Label part of the stack for local variable
        menu_stats EQU [ebp - menu_stats_nbytes]; 

    ;// 
        PUSH eax;
        PUSH ebx;
        PUSH ecx;
        PUSH edx;
        PUSH esi;

    ;// Populate local variables

        COMMENT /*
        menu_stats  DWORD OFFSET menu_stats_title
                    DWORD OFFSET menu_stats_opt_game_count
                    DWORD OFFSET menu_stats_opt_game_count_won
                    DWORD OFFSET menu_stats_opt_game_count_tie
                    DWORD OFFSET menu_stats_opt_game_count_lost
                    DWORD OFFSET menu_stats_opt_game_count_pvc
                    DWORD OFFSET menu_stats_opt_game_count_pvc_user_win
                    DWORD OFFSET menu_stats_opt_game_count_pvc_comp_win
                    DWORD OFFSET menu_stats_opt_game_count_pvc_tie
                    DWORD OFFSET menu_stats_opt_game_count_cvc
                    DWORD OFFSET menu_stats_opt_game_count_cvc_comp1_win
                    DWORD OFFSET menu_stats_opt_game_count_cvc_comp2_win
                    DWORD OFFSET menu_stats_opt_game_count_cvc_tie
                    DWORD OFFSET menu_stats_opt_all
                    DWORD OFFSET menu_stats_opt_exit
                    DWORD 0; */

            LEA edx, menu_stats;

            MOV esi, OFFSET menu_stats_title;
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_won
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_tie
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_lost
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_pvc
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_pvc_user_win
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_pvc_comp_win
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_pvc_tie
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_cvc
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_cvc_comp1_win
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_cvc_comp2_win
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_game_count_cvc_tie
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_all
            MOV [edx], esi;
            ADD edx, 4;
            MOV ESI, OFFSET menu_stats_opt_exit
            MOV [edx], esi;
            ADD edx, 4;
            MOV DWORD PTR [EDX], 0;


    stats_menu_loop:

        ;//  
            CALL CLRSCR;

        ;// PRINT_MENU_AND_GET_SELECTION
            LEA esi, menu_stats;
            INVOKE PRINT_MENU_AND_GET_SELECTION, esi;

        ;// Process menu selection
        ;// If user chose to quit, then quit
            CMP ecx, menu_stats_option_num_of_quit;
            JZ done;

            MOV eax, 0;
            L2:

                CMP eax, menu_stats_numel - 4 + 1;
                ;// -1 because first element of menu_stats is the menu title
                ;// -1 because last element of menu_stats is a terminating null
                ;// -1 because second to element of menu_stats is exit/quit
                ;// -1 because third to element of menu_stats is display all stats
               
                JZ L2_end;

                CMP ecx, eax;
                JNZ end_if;
                    CALL CLRSCR;

                    MOV eax, ecx;
                    MOV bl, (TYPE DWORD)
                    MUL bl;
                    LEA edx, menu_stats;
                    ADD edx, eax;
                    MOV eax, [edx];
                    MOV edx, eax;
                    CALL WRITESTRING; (string edx)

                    CALL CRLF;
                    MOV eax, ecx;
                    DEC eax;
                    MOV bl, (TYPE DWORD)
                    MUL bl;
                    MOV esi, offset_of_stats_all;
                    ADD esi, eax;
                    MOV eax, [esi];
                    CALL WRITEDEC;(int eax)
                    CALL CRLF;
                    CALL CRLF;
                    CALL WAITMSG;
                    JMP stats_menu_loop;
                end_if:
                INC eax;
            JMP L2;
            L2_end: 
            
            INVOKE DISPLAY_STATS_ALL, offset_of_stats_all;
            CALL WAITMSG;

    JMP stats_menu_loop;
       
    done:
        POP esi;
        POP edx;
        POP ecx;
        POP ebx;
        POP eax;
        RET;

DISPLAY_STATS_SUBMENU ENDP;

COMMENT /************************************************************
        DISPLAY_STATS_ALL

        Description:           Displays various statistics such as
                               the total number of matches played,
                               the total number of matches won by the
                               user in user versus computer mode,
                               etc...

        Preconditions:         STACK: Is not full.
                                      On the top of the stack, we have
                                      1 DWORD:

                               offset_of_stats_all:

      [offset_of_stats_all + 0 *4] == game count
      [offset_of_stats_all + 1 *4] == game_count_won
      [offset_of_stats_all + 2 *4] == game_count_tie
      [offset_of_stats_all + 3 *4] == game_count_lost
      [offset_of_stats_all + 4 *4] == game_count user versus computer
      [offset_of_stats_all + 5 *4] == game_count user versus computer_user_win
      [offset_of_stats_all + 6 *4] == game_count user versus computer_comp_win
      [offset_of_stats_all + 7 *4] == game_count user versus computer_tie 
      [offset_of_stats_all + 8 *4] == game_count computer v computer 
      [offset_of_stats_all + 9 *4] == game_count computer v computer_comp1_win
      [offset_of_stats_all + 10*4] == game_count computer v computer_comp2_win
      [offset_of_stats_all + 11*4] == game_count computer v computer_tie                               

        Postconditions:         STACK: Input to procedure (1 DWORD) has
                                       been removed from the stack.

                              CONSOLE: statistics were displayed
                                       to the console.    

*********************************************************************/
DISPLAY_STATS_ALL PROC, offset_of_stats_all:DWORD
    .data

        str_stat_description_game_count                BYTE "Total number of matches played                           : ", 0;
        str_nbytes = ($ - str_stat_description_game_count);          
        str_stat_description_game_count_won            BYTE "Total number of matches won                              : ", 0;          
        str_stat_description_game_count_tie            BYTE "Total number of matches tied                             : ", 0;        
        str_stat_description_game_count_lost           BYTE "Total number of matches lost                             : ", 0;        
        str_stat_description_game_count_pvc            BYTE "Total number of matches    played    in user vs comp mode: ", 0;           
        str_stat_description_game_count_pvc_user_win   BYTE "Total number of matches won by user  in user vs comp mode: ", 0;  
        str_stat_description_game_count_pvc_comp_win   BYTE "Total number of matches won by comp  in user vs comp mode: ", 0;  
        str_stat_description_game_count_pvc_tie        BYTE "Total number of matches     tied     in user vs comp mode: ", 0;    
        str_stat_description_game_count_cvc            BYTE "Total number of matches    played    in comp vs comp mode: ", 0; 
        str_stat_description_game_count_cvc_comp1_win  BYTE "Total number of matches won by comp1 in comp vs comp mode: ", 0; 
        str_stat_description_game_count_cvc_comp2_win  BYTE "Total number of matches won by comp2 in comp vs comp mode: ", 0;
        str_stat_description_game_count_cvc_tie        BYTE "Total number of matches     tied     in comp vs comp mode: ", 0;
        str_count = ($ - str_stat_description_game_count) / str_nbytes;

    .code

    ;//
        PUSH eax;
        PUSH ecx;
        PUSH edx;
        PUSH esi;

    ;//
        CALL CLRSCR;
        MOV esi, offset_of_stats_all;
        MOV edx, OFFSET str_stat_description_game_count;
        MOV ecx, str_count
        loop_head:
            CALL WRITESTRING;(strign edx)
            MOV eax, [esi];
            CALL WRITEDEC;(int eax)
            CALL CRLF;
            ADD esi, 4;
            ADD edx, str_nbytes;
        LOOP loop_head;

    ;//
        POP esi;
        POP edx;
        POP ecx;
        POP eax;
        RET;

DISPLAY_STATS_ALL ENDP;

COMMENT /************************************************************
        UPDATE_STATS

        Description:           Updates statistics on how many matches
                               have been played. how many matches were
                               won by the user, etc...                                   

        Preconditions:         STACK: stack is not full.
                                      On the top of the stack, we have
                                      3 DWORDS:

                               offset_of_stats_all:

      [offset_of_stats_all + 0 *4] == game count
      [offset_of_stats_all + 1 *4] == game_count_won
      [offset_of_stats_all + 2 *4] == game_count_tie
      [offset_of_stats_all + 3 *4] == game_count_lost
      [offset_of_stats_all + 4 *4] == game_count user versus computer
      [offset_of_stats_all + 5 *4] == game_count user versus computer_user_win
      [offset_of_stats_all + 6 *4] == game_count user versus computer_comp_win
      [offset_of_stats_all + 7 *4] == game_count user versus computer_tie 
      [offset_of_stats_all + 8 *4] == game_count computer v computer 
      [offset_of_stats_all + 9 *4] == game_count computer v computer_comp1_win
      [offset_of_stats_all + 10*4] == game_count computer v computer_comp2_win
      [offset_of_stats_all + 11*4] == game_count computer v computer_tie 


                               pvc_or_cvc: 1 if match played was
                                             user versus computer

                                           2 if match played was
                                             computer versus
                                             computer.

                               who_won:

                                   if pvc_or_cvc == 1

                                       1 if user won
                                       2 if user tied with computer
                                       3 if computer won.

                                   if pvc_or_cvc == 2

                                       1 if computer 1 won
                                       2 if computer tied with itself
                                       3 if computer 2 won.
                              

        Postconditions:         STACK: Inputs (3 DWORDs) have been removed
                                       from the stack.

                  offset_of_stats_all: Apropriate fields have been
                                       incremented

*********************************************************************/
UPDATE_STATS PROC, offset_of_stats_all:DWORD, pvc_or_cvc:DWORD, who_won:DWORD

    ;//
        PUSH eax;
        PUSH ecx;
        PUSH edx;
        PUSH esi;

    ;//
        CMP pvc_or_cvc, 1; ;//  1: Player vs computer
        JNZ play_c_vs_c;

        ;// who_won: 1 if user won
        ;//          2 if user tied with computer
        ;//          3 if computer won.

        MOV edx, offset_of_stats_all;
        INC DWORD PTR [edx + 0*(TYPE DWORD)]; // game count
        INC DWORD PTR [edx + 4*(TYPE DWORD)]; // game_count_player vs computer
        CMP who_won, 2;
        JNZ not_tie;
            INC DWORD PTR[edx + 2*(TYPE DWORD)]; // game count tie
            INC DWORD PTR[edx + 7*(TYPE DWORD)]; // game_count user versus computer_tie 
            JMP done;
        not_tie:
            INC DWORD PTR [edx + 1*(TYPE DWORD)]; // game count won
            INC DWORD PTR [edx + 3*(TYPE DWORD)]; // game count lost
        CMP who_won, 1;
        JNZ comp_win;
            INC DWORD PTR[edx + 5*(TYPE DWORD)]; // game_count user versus computer_user_win
            JMP done;
        comp_win:       
           INC DWORD PTR [edx + 6*(TYPE DWORD)]; // game_count user versus computer_comp_win
           JMP done;                       

        play_c_vs_c:
        
        ;// who_won: 1 if computer 1 won
        ;//          2 if computer tied with itself
        ;//          3 if computer 2 won.

        MOV edx, offset_of_stats_all;
        INC DWORD PTR [edx + 0*(TYPE DWORD)]; // game count
        INC DWORD PTR [edx + 8*(TYPE DWORD)]; // game_count computer vs computer
        CMP who_won, 2;
        JNZ not_tie2;
            INC DWORD PTR[edx + 2*(TYPE DWORD)];  // game count tie
            INC DWORD PTR[edx + 11*(TYPE DWORD)]; // game_count computer vs computer tie 
            JMP done;
        not_tie2:
            INC DWORD PTR [edx + 1*(TYPE DWORD)]; // game count won
            INC DWORD PTR [edx + 3*(TYPE DWORD)]; // game count lost
        CMP ecx, 1;
        JNZ comp2_win;
            INC DWORD PTR[edx + 9*(TYPE DWORD)]; // game_count computer one win
            JMP done;
        comp2_win:       
           INC DWORD PTR [edx + 10*(TYPE DWORD)]; // game_count computer two win

    done:
        POP esi;
        POP edx;
        POP ecx;
        POP eax;
        RET;

UPDATE_STATS ENDP;

COMMENT /************************************************************
        PLAY_A_ROUND_OF_COMP_VS_COMP

        Description:           Computer plays tic-tak-toe with itself.

        Preconditions:         Stack is not full.

        Postconditions:        The computer played a round of
                               tic-tak-toe with itself.

                               Results after each move were displayed
                               on the console for 2 seconds.

                               If Randomize was not called before
                               PLAY_A_ROUND_OF_COMP_VS_COMP, then repeated
                               calls to PLAY_A_ROUND_OF_COMP_VS_COMP will
                               display the same results.

                               If Randomize was called before calling
                               PLAY_A_ROUND_OF_COMP_VS_COMP, then computer
                               moves may vary.

                          ECX: 1 if computer 1 won
                               2 if computer tied with itself
                               3 if computer 2 won.

*********************************************************************/
PLAY_A_ROUND_OF_COMP_VS_COMP PROC
    ;// Reserve memory on the stack for local variables

        game_board_COMP_V_COMP_nbytes  = 50; 
        msg_its_a_tie_CVC_nbytes = 50;
        msg_computer_1_win_nbytes  = 50;
        msg_computer_2_win_nbytes  = 50;

        total_nbytes = game_board_COMP_V_COMP_nbytes + msg_its_a_tie_CVC_nbytes + msg_computer_1_win_nbytes + msg_computer_2_win_nbytes;
        
        ;// want to end on a double word boundary
        ;// dword_count = CEILING (total_nbytes / 4);
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        PUSH ebp;     // only automatically generated by invoke if the procedure has at least one input parameter
        MOV ebp, esp; // only automatically generated by invoke if the procedure has at least one input parameter

        dword_count = 1 + ((total_nbytes - 1) / 4);
        SUB esp, 4*dword_count;

    ;// Label parts of the stack for local variables

        game_board_COMP_V_COMP  EQU [ebp - game_board_COMP_V_COMP_nbytes ];
        msg_its_a_tie_CVC EQU [ebp - (game_board_COMP_V_COMP_nbytes + msg_its_a_tie_CVC_nbytes)];
        msg_computer_1_win  EQU [ebp - (game_board_COMP_V_COMP_nbytes  + msg_its_a_tie_CVC_nbytes  + msg_computer_1_win_nbytes )];
        msg_computer_2_win  EQU [ebp - total_nbytes];

    ;// 
        PUSH eax;
        PUSH ebx;
        PUSH edx;
        PUSH esi;

    ;// Populate local variables

        ;// LOCAL msg_its_a_tie_CVC BYTE "The match ended in a tie.", 0;

            LEA edx, msg_its_a_tie_CVC

            MOV BYTE PTR [edx], 'T';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;
            INC edx;

        ;// LOCAL msg_computer_1_win BYTE "Computer 1 wins.", 0;

            LEA edx, msg_computer_1_win

            MOV BYTE PTR [edx], 'C';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], '1';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;           

        ;// LOCAL msg_computer_2_win BYTE "computer2 wins.", 0;   

            LEA edx, msg_computer_2_win

            MOV BYTE PTR [edx], 'C';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], '2';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;  


        ;// LOCAL game_board_COMP_V_COMP BYTE 9 DUP(' ');

            LEA edx, game_board_COMP_V_COMP

            MOV BYTE PTR [edx], ' '; // 1
            INC edx;
            MOV BYTE PTR [edx], ' '; // 2
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 3
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 4
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 5
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 6
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 7
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 8
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 9     

    ;// Finished populating local variables. Now do other stuff:

        time_delay = 1000; // 1 second = 1,000 milliseconds

    ;// 
        LEA esi, game_board_COMP_V_COMP;

    ;// ebx holds symbol used on a move. Make imaginary previous move a naught
    ;// (previous move is imaginary because there is no previous move when the match first starts)
        MOV ebx, 'O';

    ;// Use ecx to count how many moves have been made so far
        MOV ecx, 0;

    ;// Decide which player will go first.
        INVOKE RANDOM_CARRY_FLAG;
        JC computer2_plays;
        ;// JMP computer2_plays;// debugging statement

    ;// 
        loop_header:

            ;// If 9 or more moves have been made, exit the loop
                CMP ecx, 9;
                JNC loop_end;

            ;// computer_1_plays:

                ;// Play symbol (naught or cross) opposite of what previously played.
                    CMP ebx, 'O'
                    JNZ computer_1_play_oh;
                    MOV ebx, 'X';
                    JMP done_deciding_naught_or_cross;
                    computer_1_play_oh:
                    MOV ebx, 'O';
                    done_deciding_naught_or_cross:
 
                ;// [eax, edx] <-
                    INVOKE GET_COMPUTER_MOVE, esi;  

                    INVOKE MAKE_MOVE, esi  , eax, edx   , ebx;
                ;// INVOKE MAKE_MOVE, board, row, column, symbol;

            ;// check whether computer_1 just played a winning move
                ;// eax <-
                    INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, esi          , eax, edx
                ;// INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, tic_tac_board, row, col
                ;//
                    CMP eax, 0;
                    JNZ computer_1_win;

            ;// increment how many moves have been made so far
                INC ecx;

            ;// 
                CALL CLRSCR;
                INVOKE PRINT_BOARD, esi;
                INVOKE WAIT_MILLISECONDS, time_delay;

            ;// If 9 or more moves have been made, exit the loop
                CMP ecx, 9;
                JZ loop_end;

            computer2_plays:
 
                ;// [eax, edx] <-
                    INVOKE GET_COMPUTER_MOVE, esi;
                
                ;// Play symbol opposite of that symbol previously used.
                    CMP ebx, 'O'
                    JNZ make_oh;
                    MOV ebx, 'X';
                    JMP make_comp_move;
                    make_oh:
                    MOV ebx, 'O';
                
                make_comp_move:
                   INVOKE MAKE_MOVE, esi  , eax, edx   , ebx;
               ;// INVOKE MAKE_MOVE, board, row, column, symbol;
            
            ;// Check if computer2 just won or not
               ;// eax <-
                   INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, esi, eax, edx;
               ;//
                   CMP eax, 0; 
                   JNZ comp2_win;

            ;// increment how many moves have been made so far
                INC ecx; 

            ;// 
                CALL CLRSCR;
                INVOKE PRINT_BOARD, esi;
                INVOKE WAIT_MILLISECONDS, time_delay;               

        JMP loop_header;
        loop_end:

    ;// it's a tie.
        MOV ecx, 2;
        LEA edx, msg_its_a_tie_CVC;
        JMP print_out_who_won;

    computer_1_win:
        MOV ecx, 1;
        LEA edx, msg_computer_1_win;
        JMP print_out_who_won;

    comp2_win:
        MOV ecx, 3;
        LEA edx, msg_computer_2_win;

    print_out_who_won:
        CALL CLRSCR;
        INVOKE PRINT_BOARD_WITH_SOME_HIGHLIGHTED, esi, eax;
        CALL CRLF;
        CALL WRITESTRING;(string edx)
        CALL CRLF;

    done: 

        POP esi;
        POP edx;
        POP ebx;
        POP eax;

        MOV esp, ebp; // only automatically generated by invoke if procedure has at least one input parameter
        POP ebp;      // only automatically generated by invoke if procedure has at least one input parameter

        RET;

PLAY_A_ROUND_OF_COMP_VS_COMP ENDP;

COMMENT /************************************************************
        PLAY_A_ROUND_OF_COMP_VS_USER

        Description:      User prompted to play a match of
                          tic-tak-toe against the computer.

        Preconditions:    Stack not full

        Postconditions:   User played a match of tic-tak-toe
                          against the computer.

                          ECX: 1 if user won
                               2 if user tied with computer
                               3 if computer won.

*********************************************************************/
PLAY_A_ROUND_OF_COMP_VS_USER PROC
    ;// Reserve memory on the stack for local variables

        game_board_1_nbytes  = 10 + 9; 
        msg_its_a_tie_nbytes = 10 + 27;
        msg_user_win_nbytes  = 10 + 19;
        msg_comp_win_nbytes  = 10 + 15;

        ;// + 10s are to reserve slightly extra space in case I miscounted the number of characters in each local string.

        total_nbytes = game_board_1_nbytes + msg_its_a_tie_nbytes + msg_user_win_nbytes + msg_comp_win_nbytes;
        
        ;// want to end on a double word boundary
        ;// dword_count = CEILING (total_nbytes / 4);
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        PUSH ebp;     // only automatically generated by invoke if there is at least one input parameter
        MOV ebp, esp; // only automatically generated by invoke if there is at least one input parameter

        dword_count = 1 + ((total_nbytes - 1) / 4);
        SUB esp, 4*dword_count;

    ;// Label parts of the stack for local variables

        game_board_1  EQU [ebp - game_board_1_nbytes ];
        msg_its_a_tie EQU [ebp - (game_board_1_nbytes + msg_its_a_tie_nbytes)];
        msg_user_win  EQU [ebp - (game_board_1_nbytes  + msg_its_a_tie_nbytes  + msg_user_win_nbytes )];
        msg_comp_win  EQU [ebp - (total_nbytes)];

    ;// 
        PUSH eax;
        PUSH ebx;
        PUSH edx;
        PUSH esi;

    ;// Populate local variables

        ;// LOCAL msg_its_a_tie BYTE "The match ended in a tie.", 0;

            LEA edx, msg_its_a_tie

            MOV BYTE PTR [edx], 'T';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;
            INC edx;

        ;// LOCAL msg_user_win BYTE "Human player wins.", 0; // 19  characters


            LEA edx, msg_user_win


            MOV BYTE PTR [edx], 'H';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;            


        ;// LOCAL msg_comp_win BYTE "Computer wins.", 0; // 15  characters     

            LEA edx, msg_comp_win

            MOV BYTE PTR [edx], 'C';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;  

        ;// LOCAL game_board_1 BYTE 9 DUP(' ');

            LEA edx, game_board_1

            MOV BYTE PTR [edx], ' '; // 1
            INC edx;
            MOV BYTE PTR [edx], ' '; // 2
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 3
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 4
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 5
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 6
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 7
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 8
            INC edx;
            MOV BYTE PTR[edx], ' ';  // 9      


    ;// Finished populating local variables. Now do other stuff:

    ;// 
        LEA esi, game_board_1;

    ;// ebx holds symbol used on a move. Make imaginary previous move a naught
    ;// (previous move is imaginary because there is no previous move when the match first starts)
        MOV ebx, 'O';

    ;// Use ecx to count how many moves have been made so far
        MOV ecx, 0;

    ;// Decide which player will go first.
        INVOKE RANDOM_CARRY_FLAG;
        JC computer_plays;
        ;// JMP computer_plays;// debugging statement

    ;// 
        loop_header:

            ;// If 9 or more moves have been made, exit the loop
                CMP ecx, 9;
                JNC loop_end;

            ;// user_plays:

                ;// Play symbol (naught or cross) opposite of what previously played.
                    CMP ebx, 'O'
                    JNZ user_play_oh;
                    MOV ebx, 'X';
                    JMP done_deciding_naught_or_cross;
                    user_play_oh:
                    MOV ebx, 'O';
                    done_deciding_naught_or_cross:
 
                ;// [eax, edx] <-
                    INVOKE GET_USER_MOVE, esi, ebx;  

                    INVOKE MAKE_MOVE, esi  , eax, edx   , ebx;
                ;// INVOKE MAKE_MOVE, board, row, column, symbol;

            ;// check whether user just played a winning move
                ;// eax <-
                    INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, esi          , eax, edx
                ;// INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, tic_tac_board, row, col
                ;//
                    CMP eax, 0;
                    JNZ user_win;

            ;// increment how many moves have been made so far
                INC ecx;

            ;// If 9 or more moves have been made, exit the loop
                CMP ecx, 9;
                JZ loop_end;

            computer_plays:
 
                ;// [eax, edx] <-
                    INVOKE GET_COMPUTER_MOVE, esi;
                
                ;// Play symbol opposite of that symbol previously used.
                    CMP ebx, 'O'
                    JNZ make_oh;
                    MOV ebx, 'X';
                    JMP make_comp_move;
                    make_oh:
                    MOV ebx, 'O';
                
                make_comp_move:
                   INVOKE MAKE_MOVE, esi  , eax, edx   , ebx;
               ;// INVOKE MAKE_MOVE, board, row, column, symbol;
            
            ;// Check if computer just won or not
               ;// eax <-
                   INVOKE IS_SQUARE_PART_OF_3_IN_A_ROW, esi, eax, edx;
               ;//
                   CMP eax, 0; 
                   JNZ comp_win;

            ;// increment how many moves have been made so far
                INC ecx; 

        JMP loop_header;
        loop_end:

    ;// it's a tie.
        MOV ecx, 2;
        LEA edx, msg_its_a_tie;
        JMP print_out_who_won;

    user_win:
        MOV ecx, 1;
        LEA edx, msg_user_win;
        JMP print_out_who_won;

    comp_win:
        MOV ecx, 3;
        LEA edx, msg_comp_win;

    print_out_who_won:
        CALL CLRSCR;
        INVOKE PRINT_BOARD_WITH_SOME_HIGHLIGHTED, esi, eax;
        CALL CRLF;
        CALL WRITESTRING;(string edx)
        CALL CRLF;

    done: 

        POP esi;
        POP edx;
        POP ebx;
        POP eax;

        MOV esp, ebp; // only automatically generated by invoke if procedure has at least one input parameter
        POP ebp;      // only automatically generated by invoke if procedure has at least one input parameter

        RET;

PLAY_A_ROUND_OF_COMP_VS_USER ENDP;

COMMENT /************************************************************
        ASK_USER_IF_THEY_WANT_TO_GO_AGAIN

        Description:      User is asked user if they want to play
                          again . 

        Preconditions:    STACK:     not full.

        Postconditions:   ZERO FLAG: clear if user indicated
                                     that they want to play
                                     again.
                                     
                                     set if user indicated
                                     that they do not  wish to
                                     play any further. 

*********************************************************************/
ASK_USER_IF_THEY_WANT_TO_GO_AGAIN PROC

    ;// Reserve memory on the stack for local variables
        menu_play_again_title_nbytes   = 40; // "Do you want to play again?", 0;
        menu_play_again_opt_yes_nbytes = 5; //  "Yes", 0;
        menu_play_again_opt_no_nbytes  = 3; //  "No", 0;

        menu_play_again_numel = 4;
        ;// 1 element  for offset of menu title (Do you want to play again?")
        ;// 2 elements for offset of menu options (yes/no)
        ;// 1 element for terminating null.

        menu_play_again_option_num_of_quit = menu_play_again_numel - 2;

        menu_play_again_nbytes = menu_play_again_numel * (TYPE DWORD);

        total_nbytes =  menu_play_again_title_nbytes + menu_play_again_opt_yes_nbytes +  menu_play_again_opt_no_nbytes + menu_play_again_nbytes;
        
        ;// want to end on a double word boundary
        ;// dword_count = CEILING (total_nbytes / 4);
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        PUSH ebp;     // only automatically generated by invoke if the procedure has at least one input parameter
        MOV ebp, esp; // only automatically generated by invoke if the procedure has at least one input parameter

        dword_count = 1 + ((total_nbytes - 1) / 4);
        SUB esp, 4*dword_count;

    ;// Label parts of the stack for local variables


        menu_play_again_title   EQU [ebp - (menu_play_again_title_nbytes)];
        menu_play_again_opt_yes EQU [ebp - (menu_play_again_title_nbytes + menu_play_again_opt_yes_nbytes)];
        menu_play_again_opt_no  EQU [ebp - (menu_play_again_title_nbytes + menu_play_again_opt_yes_nbytes +  menu_play_again_opt_no_nbytes)];
        menu_play_again         EQU [ebp - total_nbytes];


    ;// 
        PUSH eax;
        PUSH ebx;
        PUSH ecx;
        PUSH edx;
        PUSH esi;

    ;// Populate local variables


        ;// LOCAL menu_play_again_title    BYTE "Do you want to play again?", 0;
        ;// LOCAL menu_play_again_opt_yes  BYTE "Yes", 0;
        ;// LOCAL menu_play_again_opt_no   BYTE "No", 0;
        ;// LOCAL menu_play_again          DWORD OFFSET  menu_play_again_title, OFFSET menu_play_again_opt_yes, OFFSET menu_play_again_opt_no, 0;


            LEA edx, menu_play_again_title

            MOV BYTE PTR [edx], 'D';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'g';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], '?';
            INC edx;
            MOV BYTE PTR [edx], 0;

        ;// LOCAL menu_play_again_opt_yes  BYTE "Yes", 0;

            LEA edx, menu_play_again_opt_yes

            MOV BYTE PTR [edx], 'Y';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 0;   

        ;// LOCAL menu_play_again_opt_no  BYTE "No", 0;

            LEA edx, menu_play_again_opt_no

            MOV BYTE PTR [edx], 'N';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 0;            

        ;// LOCAL menu_play_again  DWORD OFFSET  menu_play_again_title, OFFSET menu_play_again_opt_yes, OFFSET menu_play_again_opt_no, 0;

            LEA edx, menu_play_again

            LEA esi, menu_play_again_title;
            MOV DWORD PTR [edx], esi;

            ADD edx, 4;
            LEA esi, menu_play_again_opt_yes;
            MOV DWORD PTR [edx], esi;

            ADD edx, 4;
            LEA esi, menu_play_again_opt_no;
            MOV DWORD PTR [edx], esi;

            ADD edx, 4;
            MOV DWORD PTR [edx], 0;  


    ;// Print_menu_and_get_selection:
        LEA esi, menu_play_again;
        ;// computer_1 selection ecx <-
            INVOKE PRINT_MENU_AND_GET_SELECTION, esi;

    ;// Process menu selection

        ;// If computer_1 chose to quit, then quit
            CMP ecx, menu_play_again_option_num_of_quit;

    done: 

        POP esi;
        POP edx;
        POP ecx;
        POP ebx;
        POP eax;

        MOV esp, ebp; // only automatically generated by invoke if procedure has at least one input parameter
        POP ebp;      // only automatically generated by invoke if procedure has at least one input parameter

        RET;

ASK_USER_IF_THEY_WANT_TO_GO_AGAIN ENDP;

COMMENT /************************************************************
        IS_SQUARE_PART_OF_3_IN_A_ROW

        Description:   Checks whether a position on a tic-tak-toe
                       board is part of a winning streak of 3
                       or not.

        Preconditions:    STACK: On the top of the stack, we have
                                 three DWORDS:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.

                           row:  Indicates the row in which the
                                 board position to be checked is
                                 located. Row indexing begins with
                                 zero. Row must take on a value of
                                 0, 1, or 2.

                           col: Indicates the column in which the
                                board position is located. Column
                                indexing begins with zero. col must
                                take on a value of 0, 1, or 2.

        Postconditions:   STACK: Procedure inputs (3 DWORDs) have
                                 been removed from the stack.

                            EAX: 0 if the specified game board position
                                 is not part of any winning streak.

                                 Otherwise, EAX is a  bit-mapped set
                                 containing data about one particulair
                                 winning streak. The least significant
                                 bit is set  if the board position with
                                 linear index 0 is in the winning streak,
                                 the second least significant bit is set
                                 if the board  position with linear index
                                 1 is in the winning streak, and so forth.
                                 In general, the nth least significant
                                 bit is set if the board position with
                                 linear index (n - 1) is in the winning
                                 streak.

                                Data for only oen winning streak is provided.
                                For example, if the board location specified
                                by procedure inputs row and col is part of a
                                horizontal winning and a verticle winning
                                streak, only one streak or the other
                                will be stored in EAX.

*********************************************************************/
IS_SQUARE_PART_OF_3_IN_A_ROW PROC, tic_tac_board:DWORD, row:DWORD, col:DWORD

    ;// reserve memory on the stack for local variables
        diagonals_to_check_nbytes = 9;
        
        ;// want to end on a double word boundary
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)
        ;// dword_count = CEILING (diagonals_to_check_nbytes / 4);

        dword_count = 1 + ((diagonals_to_check_nbytes - 1) / 4);
        SUB esp, 4*dword_count;

    ;// Label part of the stack for local variable
        diagonals_to_check  EQU [ebp - diagonals_to_check_nbytes];

    ;//
        PUSH ecx;
        PUSH esi;

    ;// Populate local variable

        ;// LOCAL diagonals_to_check BYTE 1, 0, 2, 0, 3, 0, 2, 0, 1;
                                      ;// 0  1  2  3  4  5  6  7  8 

        COMMENT /* 
        
         0 | 1 | 2
         ==|===|===
         3 | 4 | 5
         ==|===|===
         6 | 7 | 8

         0 means that don't have to check any diagonals to check for a winning streak.

         1 means only have to check the following diagonal for a winning streak:

         X |   |  
         ==|===|===
           | X |  
         ==|===|===
           |   | X

         2 means only have to check the following diagonal for a winning streak:

           |   | X
         ==|===|===
           | X |  
         ==|===|===
         X |   |  

         3 means have to check both diagonals for a winning streak. */

            LEA esi, diagonals_to_check;
            MOV BYTE PTR [esi], 1;
            INC esi;
            MOV BYTE PTR [esi], 0;
            INC esi;
            MOV BYTE PTR [esi], 2;
            INC esi;
            MOV BYTE PTR [esi], 0;
            INC esi;
            MOV BYTE PTR [esi], 3;
            INC esi;
            MOV BYTE PTR [esi], 0;
            INC esi;
            MOV BYTE PTR [esi], 2;
            INC esi;
            MOV BYTE PTR [esi], 0;
            INC esi;
            MOV BYTE PTR [esi], 1;            

    ;// check row for a winning streak.
        ;// eax <-
            INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, row, 0;
        MOV esi, tic_tac_board;
        ADD esi, eax;
        ;// esi is currently the mem offset of the square in the leftmost column of this row
        MOV al, [esi];
        ;// al contains a copy of the symbol stored in the leftmost column of this row
        CMP al, ' ';
        JZ check_column;
        CMP al, [esi + 1] ;// compare symbol in left column to symbol in middle column
        JNZ check_column; // if not the same, check column for winning streak.
        CMP al, [esi + 2];// compare symbol in left column to symbol in right column
        JNZ check_column; // if not the same, check column for winning streak.
        ;// if have reached this point, there is a winning streak in the current row

        ;//         111b; top row:    0 1 2
        ;//     111 000b; mid row:    3 4 5
        ;// 111 000 000b; bottom row: 6 7 8

        MOV eax, row; // row is either 0, 1, or 2.
        MOV ecx, 3;
        MUL cl;
        MOV ecx, eax; ;// ecx now contains 3 * row (6 max)
        MOV eax, 111b;
        SHL eax, cl;
        JMP done;

    check_column:
        ;// eax <-
            INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, 0, col;
        MOV esi, tic_tac_board;
        ADD esi, eax;
        ;// esi is currently the mem offset of the square in the top row of this column
        MOV al, [esi];
        ;// al contains a copy of the symbol stored in the top row of this column
        CMP al, ' ';
        JZ check_diag;
        CMP al, [esi + 3];// compare symbol in top row to symbol in middle row
        JNZ check_diag; // if not the same, check diagonal for winning streak.
        CMP al, [esi + 6];// compare symbol in top row to symbol in bottom row
        JNZ check_diag; // if not the same, check diagonal for winning streak.
        ;// if have reached this point, then there is a winning streak in the current column

        ;//  bit-mapped set want in eax: 001001001b; // left  column: 0 3 6
        ;//  bit-mapped set want in eax: 010010010b; // mid   column: 1 4 7
        ;//  bit-mapped set want in eax: 100100100b; // right column: 2 5 8

        CMP col, 1;
        JNZ check_if_col_is_zero;
            MOV eax, 010010010b; // middle
            JMP done;
        check_if_col_is_zero:
        CMP col, 0;
        JNZ col_is_2;
            MOV eax, 001001001b; // left
            JMP done;
        col_is_2:
            MOV eax, 100100100b; // right
            JMP done;

    check_diag:

        ;// eax <-
            INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, row, col;

        ;// 
            LEA esi, diagonals_to_check;
            ADD esi, eax;
            CMP BYTE PTR [esi], 0;
            JNZ check_1;
            MOV eax, 0;
            JMP done;
            check_1:
            CMP BYTE PTR [esi], 1;
            JZ only_check_diag_1;
            CMP BYTE PTR [esi], 2;
            JZ only_check_diag_2;

        ;// check both diagonals for a winning streak
            COMMENT /*         
                 X |   |  
                 ==|===|===
                   | X |  
                 ==|===|===
                   |   | X    */
            ;// 
                MOV esi, tic_tac_board;
                ;// esi is currently the mem offset of the square with linear index 0
                MOV al, [esi];
                ;// al contains a copy of the symbol in the square with linear index 0
                CMP al, ' ';
                JZ only_check_diag_2;               
                CMP al, [esi + 4];// compare symbol in upper left corner to center square
                JNZ only_check_diag_2;                
                CMP al, [esi + 8];// compare symbol in upper left corner to bottom right corner
                JNZ only_check_diag_2;
                ;// if have reached this point, then there is a winning streak on the diagonal
                MOV eax, 100010001b;
                JMP done;

        only_check_diag_1:
            COMMENT /*         
                 X |   |  
                 ==|===|===
                   | X |  
                 ==|===|===
                   |   | X    */
            ;// 
                MOV esi, tic_tac_board;
                ;// esi is currently the mem offset of the square with linear index 0
                MOV al, [esi];
                ;// al contains a copy of the symbol in the square with linear index 0
                CMP al, ' ';
                JNZ diag_1_mid;
                MOV eax, 0;
                JMP done;
                diag_1_mid:
                CMP al, [esi + 4];// compare symbol in upper left corner to center square
                JZ diag_1_bot;
                MOV eax, 0;
                JMP done;
                diag_1_bot:
                CMP al, [esi + 8];// compare symbol in upper left corner to bottom right corner
                JZ diag_1_win;
                MOV eax, 0;
                JMP done;
                diag_1_win:
                ;// if have reached this point, then there is a winning streak on the diagonal
                MOV eax, 100010001b;
                JMP done;

        only_check_diag_2:
            COMMENT /*         
                       |   | X 
                     ==|===|===
                       | X |  
                     ==|===|===
                     X |   |      */
            ;// 
                MOV esi, tic_tac_board;
                ;// esi is currently the mem offset of the square with linear index 0
                MOV al, [esi + 2];
                ;// al contains a copy of the symbol in the square with linear index 2
                CMP al, ' ';
                JNZ diag_2_mid;
                MOV eax, 0;
                JMP done;
                diag_2_mid:
                CMP al, [esi + 4];// compare symbol in upper right corner to center square
                JZ diag_2_bot;
                MOV eax, 0;
                JMP done;
                diag_2_bot:
                CMP al, [esi + 6];// compare symbol in upper left corner to bottom right corner
                JZ diag_2_win;
                MOV eax, 0;
                JMP done;
                diag_2_win:
                ;// if have reached this point, then there is a winning streak on the diagonal
                MOV eax, 001010100b;
                JMP done;

    done:
        POP esi;
        POP ecx;
        RET; 

IS_SQUARE_PART_OF_3_IN_A_ROW ENDP; // end procedure IS_SQUARE_PART_OF_3_IN_A_ROW

COMMENT /************************************************************
        MAKE_MOVE

        Description:   Modifies a tic-tak-toe board by placing either
                       a naught or a cross in the specified row and
                       column

        Preconditions:    STACK: On the top of the stack, we have:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.

                           row:  Indicates the row in which the
                                 board position to be filled is
                                 located. Row indexing begins with
                                 zero. row must take on a value of
                                 0, 1, or 2.

                           col: Indicates the column in which the
                                board position is located. Column
                                indexing begins with zero. col must
                                take on a value of 0, 1, or 2.

               naught_or_cross: ASCII character code of an uppercase
                                letter 'X', or an uppercase letter 
                                 'O'.

        Postconditions:   STACK: Procedure inputs (4 DWORDs) have
                                 been removed from the stack.

                  tic_tac_board: has been apropriatley modified.

*********************************************************************/
MAKE_MOVE PROC, tic_tac_board:DWORD, row:DWORD, col:DWORD, naught_or_cross:DWORD

    ;//
        PUSH eax;
        PUSH esi;

    ;// 
        INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, row, col;

    ;// 
        MOV esi, tic_tac_board;
        ADD esi, eax;
        MOV eax, naught_or_cross;
        MOV [esi], al;

        ;// none of the following work:
            ;//   MOV [esi], PTR BYTE naught_or_cross;    
            ;//   MOV [esi], BYTE PTR naught_or_cross;    
            ;//   MOV BYTE PTR [esi], BYTE PTR naught_or_cross;
            ;//   MOV BYTE PTR [esi], naught_or_cross; ;// MOV BYTE, DWORD

    done:
        POP esi;
        POP eax;
        RET; 

MAKE_MOVE ENDP; // end procedure MAKE_MOVE

COMMENT /************************************************************
        PRINT_BOARD_WITH_SOME_HIGHLIGHTED

        Description:   Prints tic-tak-toe board out to the console
                       with some board positions highlighted

        Preconditions:    STACK: On the top of the stack, we have:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array.

            which_to_highlight:  bit-mapped set where the least
                                 significant bit is set if the
                                 board position with linear index 0
                                 is to be highlighted, the second least
                                 significant bit is set if the board
                                 position with linear index 1 is to be
                                 highlighted, and so forth.                               

        Postconditions:   STACK: Inputs (2 DWORDs) have been removed
                                 from the stack.

                        CONSOLE: tic-tak-toe board pointed to by
                                 tic_tac_board was printed to the
                                 console with some board positions
                                 highlighted.

*********************************************************************/
PRINT_BOARD_WITH_SOME_HIGHLIGHTED PROC, tic_tac_board:DWORD, which_to_highlight:DWORD;

        ;//
            column_count = 3;
        ;//
            PUSH eax;
            PUSH ebx;
            PUSH ecx;
            PUSH edx;
            PUSH esi;
            
        ;// begin table on a new line
            CALL CRLF;

        ;// print column labels ("     1   2   3")
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            CALL WRITECHAR; (char al) 4
            CALL WRITECHAR; (char al) 5
            MOV al, '1';
            CALL WRITECHAR; (char al)
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            MOV al, '2';
            CALL WRITECHAR; (char al)
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            MOV al, '3';
            CALL WRITECHAR; (char al)
            CALL CRLF; // blank line

        ;// 
            CALL CRLF;

        ;// initialize count of elements printed in the current row
            MOV ebx, 0; 

        ;// 
            MOV esi, tic_tac_board;

        ;// Store count of rows printed so far in ecx
            MOV ecx, 0;


        loop_start:
            ;//

                ;// If row is full, begin a new line
                CMP ebx, column_count
                JC end_if_end_of_row

                    ;// Print verticle bar at end of row
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
        
                    ;// If row just finished was not last row, then print "   |---|---|---|"
                        CMP ecx, 2;
                        JNC print_line_break;
                        CALL CRLF;
                        MOV al, ' ';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-'
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)

                    print_line_break: 
                        CALL CRLF;

                    MOV ebx, 0; // reset count of elements in current row
                    INC ecx; // increment count  of rows printed so far
                end_if_end_of_row:

                CMP ecx, 3;
                JNC loop_end;

                ;// If this is the begining of a new row (ebx == 0)
                ;// then print row number.
                 CMP ebx, 0;
                 JNZ end_if_at_row_start;
                     MOV eax, ecx;
                     INC eax;
                     CALL WRITEDEC;(int eax)
                     MOV al, ' ';
                     CALL WRITECHAR; (char al)
                     CALL WRITECHAR; (char al)
                 end_if_at_row_start:

                ;// print current game board element  (naught, cross, or space)   
                    ;// 
                        MOV al, '|'; // part of the pound sign-like structure of the tic-tac-toe board
                        CALL WRITECHAR; (char al)
                        MOV al, ' ';
                        CALL WRITECHAR; (char al)

                    ;// check whether supposed to be highlighting the current square or not

                        ;// linear index to board position eax <-
                            INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, ecx, ebx; // ecx == row index, ebx === column index

                        ;// 
                            PUSH ecx;
                            MOV cl, al;
                            INC cl;
                            MOV eax, which_to_highlight;
                            SHR eax, cl; 
                        ;// Carry flag is currently set if we're supposed to highlight the current element
                            POP ecx;
                            PUSHFD;
                            MOV dl, 1;
                            POPFD;
                            JC get_current_console_colors;
                            MOV dl, 0
                            JMP print_current_game_board_element;                            

                    get_current_console_colors:
                        MOV eax, 0;
                        ;// [background upper AL, foreground lower AL] <-
                            CALL GETTEXTCOLOR; ()
                    ;// save old foreground and background colors for later
                        PUSH eax;

                    ;// set color to black on yellow
                        yellow = 14;
                        black = 0;
                        MOV eax, black + (yellow * 16)
                        CALL SETTEXTCOLOR; (eax)

                    print_current_game_board_element: ;// (naught, cross, or space)  
                        MOV al, BYTE PTR[esi];
                        CALL WRITECHAR; (char al)

                    ;// Decide whether to restore old colors or not
                        CMP dl, 1;
                        JNZ print_space;

                    ;// restore old foreground and background colors
                        POP eax;
                        CALL SETTEXTCOLOR; (eax)

                    print_space: 
                        MOV al, ' ';
                        CALL WRITECHAR; (char al)

                ;// increment count of values printed in current row
                    INC ebx;
            ;//
                INC esi; // Advance pointer to next element

        JMP loop_start;
        loop_end:

        POP esi;
        POP edx;
        POP ecx;
        POP ebx;
        POP eax;
        RET; 

PRINT_BOARD_WITH_SOME_HIGHLIGHTED ENDP; // end procedure PRINT_BOARD_WITH_SOME_HIGHLIGHTED

COMMENT /************************************************************
        PRINT_BOARD

        Description:   Prints tic-tak-toe board out to the console

        Preconditions:    STACK: On the top of the stack, we have:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array.

        Postconditions:   STACK: Inputs (1 DWORD) has been removed
                                 from the stack.

                        CONSOLE: tic-tak-toe board pointed to by
                                 tic_tac_board has been
                                 printed to the console.

*********************************************************************/
PRINT_BOARD PROC, tic_tac_board:DWORD

        ;//
            column_count = 3;
        ;//
            PUSH eax;
            PUSH ebx;
            PUSH ecx;
            PUSH edx;
            PUSH esi;
            
        ;// initialize count of elements printed in the current row
            MOV ebx, 0; 

        ;// 
            MOV esi, tic_tac_board;

        ;// Store count of rows printed so far in ecx
            MOV ecx, 0;

        ;// begin table on a new line
            CALL CRLF;

        ;// pritn column labels ("     1   2   3")
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            CALL WRITECHAR; (char al) 4
            CALL WRITECHAR; (char al) 5
            MOV al, '1';
            CALL WRITECHAR; (char al)
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            MOV al, '2';
            CALL WRITECHAR; (char al)
            MOV al, ' ';
            CALL WRITECHAR; (char al) 1
            CALL WRITECHAR; (char al) 2
            CALL WRITECHAR; (char al) 3
            MOV al, '3';
            CALL WRITECHAR; (char al)
            CALL CRLF; // blank line

        ;// 
            CALL CRLF;

        loop_start:
            ;//

                ;// If row is full, begin a new line
                CMP ebx, column_count
                JC end_if_end_of_row

                    ;// Print verticle bar at end of row
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
        
                    ;// If row jsut finished was not last row, then print "   |---|---|---|"
                        CMP ecx, 2;
                        JNC print_line_break;
                        CALL CRLF;
                        MOV al, ' ';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-'
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)
                        MOV al, '-';
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        CALL WRITECHAR; (char al)
                        MOV al, '|';
                        CALL WRITECHAR; (char al)

                    print_line_break: 
                        CALL CRLF;

                    MOV ebx, 0; // reset count of elements in current row
                    INC ecx; // increment count  of rows printed so far
                end_if_end_of_row:

                CMP ecx, 3;
                JNC loop_end;

                ;// If this is the begining of a new row (ebx == 0)
                ;// then print row number.
                 CMP ebx, 0;
                 JNZ end_if_at_row_start;
                     MOV eax, ecx;
                     INC eax;
                     CALL WRITEDEC;(int eax)
                     MOV al, ' ';
                     CALL WRITECHAR; (char al)
                     CALL WRITECHAR; (char al)
                 end_if_at_row_start:

                ;// print current array element     
                    MOV al, '|';
                    CALL WRITECHAR; (char al)
                    MOV al, ' ';
                    CALL WRITECHAR; (char al)
                    ;// MOVZX eax, BYTE PTR [esi] ;// alternative code
                    MOV al, BYTE PTR [esi];
                    CALL WRITECHAR; (char al)
                    MOV al, ' ';
                    CALL WRITECHAR; (char al)

                ;// increment count of values printed in current row
                    INC ebx;
            ;//
                INC esi; // Advance pointer to next element

        JMP loop_start;
        loop_end:

        POP esi;
        POP edx;
        POP ecx;
        POP ebx;
        POP eax;
        RET; 

PRINT_BOARD ENDP; // end procedure PRINT_BOARD

COMMENT $/************************************************************
        GET_USER_MOVE

        Description:   This procedure prompts the user to select one
                       of the spots or positions on the tic-tak-toe
                       board which are open or not already taken.

        Preconditions:    STACK: On the top of the stack, we have:

                 tic_tac_board4: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.
                                 At least one spot must not
                                 already be taken.

        Postconditions:   STACK: Input (1 DWORD) has been removed
                                 from the stack.
                          
                           EAX: Contains row index of a user
                                tic-tak-toe board position.
                                Is not already occupied by a
                                naught or a cross.

                           EDX: Contains column index of user
                                selected tic-tak-toe board
                                position. Is not already occupied
                                by a naught or a cross.

*********************************************************************/$
GET_USER_MOVE PROC, tic_tac_board4:DWORD, symbol:DWORD

    ;// reserve memory on the stack for local variables
        msg_please_enter_the_index_of_the_nbytes = 33;
        msg_row_nbytes = 4;
        msg_column_nbytes = 7;
        msg_you_wish_to_place_your_move_in_nbytes = 35;
        msg_err_nbytes = 95;
        msg_you_are_playing_nbytes  = 17;
        byte_total = msg_please_enter_the_index_of_the_nbytes + msg_row_nbytes + msg_column_nbytes + msg_you_wish_to_place_your_move_in_nbytes + msg_err_nbytes + msg_you_are_playing_nbytes;

        ;// want to end on a double word boundary
        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)
        ;// dword_count = CEILING (byte_total / 4);

        dword_count = 1 + ((byte_total - 1) / 4);
        SUB esp, 4*dword_count;

   ;// Label parts of the stack for local variables
        msg_please_enter_the_index_of_the  EQU [ebp - msg_please_enter_the_index_of_the_nbytes];
        msg_row                            EQU [ebp - (msg_please_enter_the_index_of_the_nbytes + msg_row_nbytes)];
        msg_column                         EQU [ebp - (msg_please_enter_the_index_of_the_nbytes + msg_row_nbytes + msg_column_nbytes)];
        msg_you_wish_to_place_your_move_in EQU [ebp - (msg_please_enter_the_index_of_the_nbytes + msg_row_nbytes + msg_column_nbytes + msg_you_wish_to_place_your_move_in_nbytes)];
        msg_err                            EQU [ebp - (msg_please_enter_the_index_of_the_nbytes + msg_row_nbytes + msg_column_nbytes + msg_you_wish_to_place_your_move_in_nbytes + msg_err_nbytes)];
        msg_you_are_playing                EQU [ebp - byte_total];

    ;//
        PUSH ecx;

    ;// Populate local variables

    ;//
        car_ret = 0Dh;
        line_feed = 0Ah;
        NULL      = 0;

        ;// LOCAL msg_please_enter_the_index_of_the BYTE 'Please enter the index of the', 0;
  
            LEA edx, msg_please_enter_the_index_of_the;
            MOV BYTE PTR [edx], car_ret;
            INC edx;
            MOV BYTE PTR [edx], line_feed;
            INC edx;
            MOV BYTE PTR [edx], 'P';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'x';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'f';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], NULL;


        ;// LOCAL msg_row BYTE 'row', 0;

            LEA edx, msg_row;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], NULL;

        ;// LOCAL column_row BYTE 'column', 0;
            LEA edx, msg_column;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], NULL;

        ;// LOCAL msg_you_wish_to_place_your_move_in BYTE 'you wish to place your move in', 0;
            LEA edx, msg_you_wish_to_place_your_move_in
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'w';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'v';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], ':';
            INC edx;
            MOV BYTE PTR [edx], car_ret;
            INC edx;
            MOV BYTE PTR [edx], line_feed;
            INC edx;
            MOV BYTE PTR [edx], NULL;

        ;// LOCAL msg_err BYTE car_ret, line_feed, 'I'm sorry, but that place on the board is already taken'
        ;//               BYTE car_ret, line_feed, 'Please choose a different location', 0;
            LEA edx, msg_err;
            MOV BYTE PTR [edx], car_ret;
            INC edx;
            MOV BYTE PTR [edx], line_feed;
            INC edx;
            MOV BYTE PTR [edx], 'I';
            INC edx;
            MOV BYTE PTR [edx], "'";
            INC edx;
            MOV BYTE PTR [edx], 'm';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], ',';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'b';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'b';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'k';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], car_ret;
            INC edx;
            MOV BYTE PTR [edx], line_feed;
            INC edx;
            MOV BYTE PTR [edx], 'P';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'h';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'd';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'f';
            INC edx;
            MOV BYTE PTR [edx], 'f';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'c';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], NULL;  


        ;// LOCAL msg_you_are_playing BYTE 'You are playing ', 0;
            LEA edx, msg_you_are_playing;
            MOV BYTE PTR [edx], 'Y';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 'u';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'y';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 'g';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], NULL;               

    ;// 
        loop_header:
                CALL CLRSCR;
                INVOKE PRINT_BOARD, tic_tac_board4;

            ;// Tell the user whether they are playing naughts or crosses.

                CALL CRLF;

                LEA edx, msg_you_are_playing;
                CALL WRITESTRING;

                MOV eax, symbol;
                CALL WRITECHAR; (char al)

                MOV al, 's';
                CALL WRITECHAR; (char al)

            ;// get_row:
                CALL CRLF;

                LEA edx, msg_please_enter_the_index_of_the;
                CALL WRITESTRING;

                LEA edx, msg_row;
                CALL WRITESTRING;

                LEA edx, msg_you_wish_to_place_your_move_in;
                CALL WRITESTRING;

           ;// Prepare prompt to pass to USER_INT_RANGE_LOOPED
           ;// want to pass an empty prompt to USER_INT_RANGE_LOOPED
           ;// point edx to the null-character occuring at the end of the local string msg_row
                LEA edx, msg_row;
                ADD edx, 3;

            ;// ecx <-
                INVOKE USER_INT_RANGE_LOOPED, 1, 3, edx;
            ;//
                MOV eax, ecx;
                DEC eax; // user-interface uses row indicies 1, 2, 3, but internal system uses 0, 1, 2.

            ;// get_column:
                CALL CLRSCR;
                INVOKE PRINT_BOARD, tic_tac_board4;
            ;//
                LEA edx, msg_please_enter_the_index_of_the
                CALL WRITESTRING;

                LEA edx, msg_column
                CALL WRITESTRING;

                LEA edx, msg_you_wish_to_place_your_move_in
                CALL WRITESTRING;

                LEA edx, msg_row
                ADD edx, 3; // edx now points to the null-character occuring after 'row'

            ;// ecx <-
                INVOKE USER_INT_RANGE_LOOPED, 1, 3, edx;

            ;//
                MOV edx, ecx;
                DEC edx; // user-interface uses row indicies 1, 2, 3, but internal system uses 0, 1, 2.;

            ;// carry flag <-
                INVOKE IS_SPOT_AVAILABLE, tic_tac_board4, eax, edx;

        JC loop_end;

            ;// print error message
                CALL CLRSCR;
                INVOKE PRINT_BOARD, tic_tac_board4;
                LEA edx, msg_err;
                CALL WRITESTRING;
                CALL CRLF;
                CALL WAITMSG;
   
                JMP loop_header;
        loop_end:

    ;// 
        POP ecx;
        RET; 

GET_USER_MOVE ENDP; // end procedure GET_USER_MOVE

COMMENT $/************************************************************
        GET_COMPUTER_MOVE

        Description:   Of the spots or positions on the tic-tak-toe
                       board which are open / not already taken, this
                       procedure selects one.

        Preconditions:    STACK: On the top of the stack, we have:

                 tic_tac_board4: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.
                                 At least one spot must not
                                 already be taken.

                         other: Randomize was called previously
                                (Random Number Generator was seeded)

        Postconditions:   STACK: Input (1 DWORD) has been removed
                                 from the stack.
                          
                           EAX: Contains row index of selected
                                tic-tak-toe board position. Is not 
                                already occupied by a naught or
                                a cross.

                           EDX: Contains column index of randomly
                                selected tic-tak-toe board position.
                                Is not already occupied by a
                                naught or a cross.

*********************************************************************/$
GET_COMPUTER_MOVE PROC, tic_tac_board4:DWORD

    ;// check whether or not center square is available
    ;// carry flag <-
        INVOKE IS_SPOT_AVAILABLE, tic_tac_board4, 1, 1;

    ;// If carry flag set, then choose center square
        JNC else_;
            MOV eax, 1;
            MOV edx, 1;
            JMP done;
        else_:

    ;// Otherwise, choose available square randomly with uniform
    ;// probability.

    ;// [eax, edx] <-
        INVOKE RAND_SELECT_AVAILABLE_SPOT, tic_tac_board4;
 
    done:

        RET; 

GET_COMPUTER_MOVE ENDP; // end procedure GET_COMPUTER_MOVE

COMMENT $/************************************************************
        RAND_SELECT_AVAILABLE_SPOT

        Description:   Of the spots, or positions, on the tic-tak-toe
                       board which are open / not already taken, this
                       procedure slects one at random with uniform
                       probability.

        Preconditions:    STACK: On the top of the stack, we have:

                 tic_tac_board3: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.
                                 At least one spot must not be
                                 already taken.

                         other: Randomize was called previously
                                (Random Number Generator was seeded)

        Postconditions:   STACK: Input (1 DWORD) has been removed
                                 from the stack.

                           EAX: Contains row index of randomly selected
                                tic-tak-toe board position which is not 
                                already occupied by a naught or
                                a cross.

                           EDX: Contains column index of randomly
                                selected tic-tak-toe board position
                                which is not  already occupied by a
                                naught or a cross.

*********************************************************************/$
RAND_SELECT_AVAILABLE_SPOT PROC, tic_tac_board3:DWORD

    ;// 
        PUSH ebx;
        PUSH ecx;
        PUSH esi;
    
    ;// ecx <-
        INVOKE COUNT_AVAILABLE_SPOTS, tic_tac_board3;

    ;//
        MOV eax, ecx;
        ;// eax <-
            CALL RANDOMRANGE;(max+1 eax)

    ;//
        MOV esi, tic_tac_board3;

    ;//
        MOV ecx, 9;

    ;//
        MOV ebx, -1;
        MOV edx, -1;

    ;//
        loop_head:
            INC ebx;

            ;// if spot is available, increment edx
            CMP BYTE PTR [esi], ' '
            JNZ end_if;
                INC edx;
            end_if:

            CMP eax, edx
            JZ loop_end;

            INC esi;
        LOOP loop_head;
        loop_end:

    ;// ebx contains linear index (0, 1, 2, 3,4 5, 6, 7, 8)
    ;// to the board position. Convert into (row, column) format

        INVOKE CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT, ebx;

        POP esi;
        POP ecx;
        POP ebx;
        RET; 

RAND_SELECT_AVAILABLE_SPOT ENDP; // end procedure RAND_SELECT_AVAILABLE_SPOT

COMMENT $/************************************************************
        COUNT_AVAILABLE_SPOTS

        Description:   Counts the number of spots or positions on
                       tic-tak-toe board which are open / not already
                       taken.

        Preconditions:    STACK: On the top of the stack, we have:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.

        Postconditions:   STACK: Input (1 DWORD) has been removed
                                 from the stack.

                           ECX: Contains count of tic-tak-toe board
                                positions which are not already
                                occupied by a naught or a cross.

*********************************************************************/$
COUNT_AVAILABLE_SPOTS PROC, tic_tac_board2:DWORD

    ;// 
        PUSH eax;
        PUSH ebx;

    ;// 
        MOV ecx, 0;

    ;// Use eax to hold row-index
        MOV eax, 0;

    ;// Use ebx to hold column-index
        MOV ebx, 0;

        L:
           ;// If row index (eax) is 3 or greater, then depart loop
               CMP eax, 3
               JNC loop_end;

            ;// 
                INVOKE IS_SPOT_AVAILABLE, tic_tac_board2, eax, ebx;

            ;// If carry flag is set, increment ecx
            JNC end_if1;
                INC ecx;
            end_if1:

            ;// if just checked a board position in the last column (column 2),
            ;// move to first column (set ebx to zero) and advance to next row (increment eax)
            ;// Otherwise, advance to next column (increment ebx)
            CMP ebx, 2;
            JC else2;
                INC eax;
                MOV ebx, 0;
                JMP end_if2;
            else2:
                INC ebx;
            end_if2:

        JMP L;
        loop_end:

        POP ebx;
        POP eax;
        RET; 

COUNT_AVAILABLE_SPOTS ENDP; // end procedure COUNT_AVAILABLE_SPOTS

COMMENT /************************************************************
        IS_SPOT_AVAILABLE

        Description:   Checks whether a position on a tic-tak-toe
                       board is already taken or not

        Preconditions:    STACK: On the top of the stack, we have:

                  tic_tac_board: Contains memory offset pointing
                                 to the begining of an array
                                 representing a tic-tac-toe board.

                           row:  Indicates the row in which the
                                 board position to be checked is
                                 located. Row indexing begins with
                                 zero. Row must take on a value of
                                 0, 1, or 2.

                           col: Indicates the column in which the
                                board position is located. Column
                                indexing begins with zero. col must
                                take on a value of 0, 1, or 2.

        Postconditions:   STACK: Inputs (3 DWORDs) have been removed
                                 from the stack.

                    Carry Flag: set if spot on board is empty
                                clear if spot on board is already
                                occupied by an naught or a cross.

*********************************************************************/
IS_SPOT_AVAILABLE PROC, tic_tac_board:DWORD, row:DWORD, col:DWORD

    ;//
        PUSH eax;
        PUSH ebx;
        PUSH esi;

    ;// eax <-
        INVOKE CONVERT_ROW_COL_TO_LINEAR_INDEX, row, col;

    ;// 
        MOV esi, tic_tac_board;


    ;// If character at position (row, col) is a space character, then move is valid.
    ;// If character at position (row, col) is not a space character
    ;// (is a naught or a cross), then mov is invalid
        ADD esi, eax;
        CMP BYTE PTR [esi], ' ';
        JNZ clear_the_carry_flag;


    ;// Set the carry flag
        STC;
        JMP done;

    clear_the_carry_flag:              
        CLC;       

    done:
        POP esi;
        POP ebx;
        POP eax;
        RET; 

IS_SPOT_AVAILABLE ENDP; // end procedure IS_SPOT_AVAILABLE

COMMENT /************************************************************
        CONVERT_ROW_COL_TO_LINEAR_INDEX

        Description:   Converts a a row-column index pair to a 
                       to a position on  a tic-tak-toe board into
                       a linear index to that same position.

        Preconditions:    STACK: On the top of the stack, we have:

                           row:  Indicates the row in which the
                                 board position to be checked is
                                 located. Row indexing begins with
                                 zero. Row must take on a value of
                                 0, 1, or 2.

                           col: Indicates the column in which the
                                board position is located. Column
                                indexing begins with zero. col must
                                take on a value of 0, 1, or 2.

        Postconditions:   STACK: Inputs (2 DWORDs) have been removed
                                 from the stack.

                            EAX: Contains linear index from
                                 {0, 1, 2, 3, 4, 5, 6, 7, 8}

*********************************************************************/
CONVERT_ROW_COL_TO_LINEAR_INDEX PROC, row:DWORD, col:DWORD

    ;//
        PUSH ebx;

    ;// 
        MOV eax, 0;

    ;// multiply row index by 3. Store in the AX register.
        MOV al, BYTE PTR row;
        MOV bl, 3;
        MUL bl;

    ;// Add column index to multiplied row index
        ADD al, BYTE PTR col;
  
    done:
        POP ebx;
        RET; 

CONVERT_ROW_COL_TO_LINEAR_INDEX ENDP; // end procedure


COMMENT $/************************************************************
        CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT

        Description:   Converts a linear index describing a
                       position on a tic-tak-toe board, which takes
                       on a value from {0, 1, 2,3 4, 5, 6, 7, 8}, to
                       a row index from {0 1, 2} and a column index
                       from {0, 1, 2}.

        Preconditions:    STACK: On the top of the stack, we have:

                   linear_index: Contains a value from
                                 {0, 1, 2, 3, 4, 5, 6, 7, 8}.

        Postconditions:   STACK: Input (1 DWORD) has been removed
                                 from the top of the stack.

                           EAX: Contains row index.
                                FLOOR(linear_index / 3)

                           EDX: Contains column index.
                                linear_index modulo 3

*********************************************************************/$
CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT PROC, linear_index:DWORD

    ;// 
        PUSH ebx;
    ;// 
        MOV edx, 0;
        MOV eax, linear_index;
    ;// 
        MOV bx, 3;

    ;// ax <- FLOOR(linear_index / 3)
    ;// dx <- linear_index MOD 3
        DIV bx;
        
    ;//
        POP ebx;
        RET; 

CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT ENDP; // end procedure CONVERT_LINEAR_INDEX_TO_ROW_COL_FORMAT

COMMENT $/************************************************************
        RANDOM_CARRY_FLAG

        Description:     Generate a random 1 or 0 with roughly equal
                         probability.

        Preconditions:   Randomize was called previously
                         (Random Number Generator was seeded)

        Postconditions:  CARRY FLAG: Is randomly clear or set

*********************************************************************/$
RANDOM_CARRY_FLAG PROC

    ;// 
        PUSH eax;

    ;// eax <-
        CALL RANDOM32;

    ;// 
        SHR al, 1;
        
    ;//
        POP eax;
        RET; 

RANDOM_CARRY_FLAG ENDP; // end procedure RANDOM_CARRY_FLAG

COMMENT $/************************************************************
        WAIT_MILLISECONDS

        Description:     Causes the program to pause for a specified
                         number of milliseconds.

        Preconditions:   STACK: On top of the stack is a DWORD
                                containing an unsigned integer which
                                represents the number of milliseconds
                                one wishes to pause the program for.
                                Is at most 40,000,000 (11.1 hours).

                         OTHER: The sum of current milliseconds
                                elapsed since midnight with the time
                                stored in the topmost DWORD of the
                                stack, does not exceed 24 hours.
                                For example, immediatly before calling
                                this procedureone does not want the
                                time since last midnight to be
                                23 hours and 59 minutes, and the sum
                                of that with the time indicated in
                                the top DWORD of the stack to total up
                                to 24 hours and 1 minute.

        Postconditions:  STACK: topmost DWORD of the stack has been
                                removed.

                         OTHER: Program has been delayed by the
                                specified number of milliseconds.

*********************************************************************/$
WAIT_MILLISECONDS PROC, time:DWORD

    ;// 
        PUSH eax;
        PUSH ebx;

    ;// eax <-
        CALL GETMSECONDS;
    ;// 
        MOV ebx, eax;

    ;// 
        loop_head:

            ;// eax <-
                CALL GETMSECONDS;

            ;// calculate how many milliseconds have passed since started
                SUB eax, ebx;

            ;// if time or more milliseconds have passed since started, then exit loop
            CMP eax, time
            JNC loop_end;
        JMP loop_head;
        loop_end:
        
    ;//
        POP ebx;
        POP eax;
        RET; 

WAIT_MILLISECONDS ENDP; // end procedure WAIT_MILLISECONDS

COMMENT /************************************************************
        USER_INT_RANGE_LOOPED

        Description:      Prompts the user to enter a positive
                          integer from some inteveral.
                          Reprompts user repeatedly untill valid
                          input is submitted.

        Preconditions:    STACK: on the top of the stack,we have:

                                 DWORD minimum acceptable integer

                                 DWORD maximum acceptable integer     

                                 DWORD mem offest of a prompt
                                 (a prompt being a null-terminated
                                 string).

        Postconditions:   CONSOLE: Prompt pointed to by edx was
                                   printed to console at least once.
                          
                          ECX: contains a user-specified integer

                          STACK: Top 3 DWORDs have been popped off
                                 the stack. 

*********************************************************************/
USER_INT_RANGE_LOOPED PROC, min:DWORD, max:DWORD, UIBL_prompt:DWORD
   
    ;// 
        PUSHFD;
    L:
        ;// [ZF, ecx] = 
            INVOKE USER_INT_BOUNDED, min, max, UIBL_prompt;
    JNZ L;

    ;// 
        POPFD;
        RET; // return

USER_INT_RANGE_LOOPED ENDP; // end procedure

COMMENT /************************************************************
        USER_INT_BOUNDED

        Description:      Prompts the user to enter a positive
                          integer from some inteveral.

        Preconditions:    STACK: on the top of the stack,we have:

                                 DWORD minimum acceptable integer

                                 DWORD maximum acceptable integer     

                                 DWORD mem offest of a prompt
                                 (a prompt being a null-terminated
                                 string).

        Postconditions:   A prompt asking the user to
                          enter a number between min and max
                          acceptable values was printed to the
                          console.

                          ZF: Set if user entered valid input.
                              ZF (zero flag) clear otherwise.                               

                          ECX: If ZF is set, ECX contains a
                               user-specified integer.

                               If ZF is clear, contents of ECX
                               are undefined.

                          STACK: Top 3 DWORDs have been popped off
                                 the stack.                                 
                               
*********************************************************************/
USER_INT_BOUNDED PROC, min:DWORD, max:DWORD, UIB_prompt:DWORD
.data
    uir_pls_enter BYTE 'Please type a whole number between ', 0;
    uir_and       BYTE ' and ', 0;
    uir_fin       BYTE 0Dh, 0Ah, 'Then press enter:', 0;

    uir_str_empty BYTE 0;

    uir_quote_mrk BYTE '"', 0;
    uir_is_small  BYTE '" is too small.', 0;
    uir_is_large  BYTE '" is too large.', 0;
.code

    ;//
        PUSH eax;
        PUSH ebx;
        PUSH edx;

    ;// 
        MOV ebx, min;
        MOV eax, max;
        MOV edx, UIB_prompt;

    ;// prompt_user:
        CALL CRLF;
        CALL WRITESTRING; (string start edx)

    ;// Print line-break
        CALL CRLF;
    
    ;// Print: "Please enter a whole number between "
        MOV edx, OFFSET uir_pls_enter;
        CALL WRITESTRING; (string start edx)

    ;// save maximum (eax) for later  ----------------,
        PUSH eax;                                     |
                                                      ;
    ;// Print: minimum (ebx)                          |            
        MOV eax, ebx;                                 |
        CALL WRITEDEC; (int eax)                      |
                                                      ;
    ;// Print: " and "                                |
        MOV edx, OFFSET uir_and;                      |
        CALL WRITESTRING; (string start edx)          |      
                                                      ;
    ;// restore maximum to eax       <----------------'
        POP eax;

    ;// Print: maximum (eax)
        CALL WRITEDEC; (int eax)

    ;// Print ": "                                
        MOV edx, OFFSET uir_fin;
        CALL WRITESTRING; (string start edx)

    ;// Get user input
        MOV edx, OFFSET uir_str_empty;
        ;// [ecx, ZF] = 
        INVOKE USER_INT, edx

    ;// check whether user input was not an integer (zero flag == 0)
        JNZ done;

    ;// check whether user input (ecx) is less than min acceptable input (ebx)
        CMP ecx, ebx
        JC too_small;

    ;// check whether max acceptable input (eax) is greater than or equal to user input
        CMP eax, ecx
        JNC valid_input;

    too_large: 
        ;// print line-break
            CALL CRLF;

        ;// print quotation mark
            MOV edx, OFFSET uir_quote_mrk;
            CALL WRITESTRING; (string start edx)

        ;// print user's input
            PUSH eax; // save eax (max acceptable input)
            MOV eax, ecx; // move user input from ecx to eax
            CALL WRITEDEC; (int eax)
            POP eax; // restore max acceptable input to eax

        ;// print end-quote and remarks about user's input
            MOV edx, OFFSET uir_is_large;
            CALL WRITESTRING; (string start edx)

        ;//  
           JMP invalid_input;

    too_small:
        ;// print line-break
            CALL CRLF;

        ;// print quotation mark
            MOV edx, OFFSET uir_quote_mrk;
            CALL WRITESTRING; (string start edx)

        ;// print user's input
            PUSH eax;
            MOV eax, ecx;
            CALL WRITEDEC; (int eax)
            POP eax;

        ;// print end-quote and remarks about user's input
            MOV edx, OFFSET uir_is_small;
            CALL WRITESTRING; (string start edx)

    invalid_input:

        ;// clear zero flag
            PUSH eax;
            OR al, 1;
            POP eax;

         ;// 
             JMP done;

    valid_input:
        ;// set zero flag
            TEST al, 0; 

    done:
        POP edx;
        POP ebx;
        POP eax;
        RET; // return

USER_INT_BOUNDED ENDP; // end procedure

COMMENT /************************************************************
        USER_INT

        Description:      Prompts the user to enter something

        Preconditions:    STACK: Top of stack is a DWORD containing
                                 the mem offest of a prompt
                                 (null-terminated string)

        Postconditions:   CONSOLE: prompt pointed to by the topmost
                                   dword on the stack was printed
                                   out to the console.

                          STACK: Top 1 DWORD has been popped off
                                 of the stack.
                          
                           ZF: Set if user entered a positive
                               integer less than 2^32 - 1

                               ZF (zero flag) clear if user enters
                               anything other than concectuive
                               characters from {0, 1, 2, 3, 4, 5, 6,
                               7, 8, 9}, with the exceptioon of
                               leading or trailing white-space
                               chars.                                       

                               If user enters "12  34", ZF will be
                               clear since digits are
                               non-concecutive.

                               If user attempts to specify that
                               their input is in decimal format
                               by apending a letter d, such as
                               "100d", ZF will be clear since
                               'd' is an illeagal character.

                               If user enters atempts to enter a
                               number in hexidecial format, such
                               as "B9" or "B9h", ZF will be
                               clear since letters are not
                               allowed characters.

                               If user enters atempts to enter a
                               non-integer, such as '2.456456',
                               ZF will be clear since '.' is not
                               an accepted character.

                               If user enters atempts to enter an
                               integer in scientific notationm, such
                               as 5 * 10^7 or 5e7, ZF will be clear
                               since '*', 'e', and '^' are not
                               accepted characters.

                               Leading white-space in user's
                               input is guaranteed to be ignored
                               up to at least 50 characters.

                               More leading white-space than that
                               and behavior of this procedure
                               is undefined.

                               All trailing white-space characters
                               are ignored, no matter how many
                               there are.

                          ECX: If ZF is set, ecx contains a
                               user-specified integer.

                               If ZF is clear, contents of ECX are
                               undefined.

*********************************************************************/
USER_INT PROC, USER_INT_prompt:DWORD; // begin procedure named "USER_INT"

    ;// reserve memory on the stack for locals
        user_string_nbytes = 100;
        err_msg_part_1_nbytes = 22;
        err_msg_part_2_nbytes = 25;
        byte_total = user_string_nbytes + err_msg_part_1_nbytes + err_msg_part_2_nbytes;

        ;// want to end on a double word boundary

        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        dword_count = 1 + ((byte_total - 1) / 4);
        SUB esp, 4*dword_count;

        user_string    EQU [ebp - user_string_nbytes];
        err_msg_part_1 EQU [ebp - (user_string_nbytes + err_msg_part_1_nbytes)];
        err_msg_part_2 EQU [ebp - (user_string_nbytes + err_msg_part_1_nbytes + err_msg_part_2_nbytes)];

    ;// 
        PUSH eax;
        PUSH edx;

    ;//
        ;// LOCAL err_msg_part_1 BYTE 'Unable to interpret "', 0;

            LEA edx, err_msg_part_1;

            MOV BYTE PTR [edx], 'U';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 'b';
            INC edx;
            MOV BYTE PTR [edx], 'l';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], '"';
            INC edx;
            MOV BYTE PTR [edx], 0;         

        ;// LOCAL err_msg_part_2 BYTE '" as a positive integer.', 0;

            LEA edx, err_msg_part_2;

            MOV BYTE PTR [edx], '"';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'a';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'p';
            INC edx;
            MOV BYTE PTR [edx], 'o';
            INC edx;
            MOV BYTE PTR [edx], 's';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'v';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], ' ';
            INC edx;
            MOV BYTE PTR [edx], 'i';
            INC edx;
            MOV BYTE PTR [edx], 'n';
            INC edx;
            MOV BYTE PTR [edx], 't';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'g';
            INC edx;
            MOV BYTE PTR [edx], 'e';
            INC edx;
            MOV BYTE PTR [edx], 'r';
            INC edx;
            MOV BYTE PTR [edx], '.';
            INC edx;
            MOV BYTE PTR [edx], 0;          

   ;// prompt user 
       CALL CRLF;
       MOV edx, USER_INT_prompt;
       CALL WRITESTRING; (string start edx)

   ;// set ecx to maximum number of characters will we allow
   ;// user to input + 1
       MOV ecx, user_string_nbytes;

   ;// get string from user:
       LEA edx, user_string; 
       ;// eax <-
           CALL READSTRING; (edx: destination of user input, ecx : max characters + 1)

    ;// process user input:

        ;// check input for illegal characters
            ;// [ZF, edx] =
            INVOKE IS_NUMERIC_STRING, edx

        ;// if user input invalid (zero_flag == 0) jump to  print_error_message
            JNZ print_error_message;

        ;// Otherwise, convert user's input from string to int
            ;// eax <-
                CALL PARSEDECIMAL32; (string start edx)

        ;// Test whether eax == 0
            CMP eax, 0;

        ;// if eax != 0, jump to valid_input_recieved
            JNZ valid_input_recieved;

        print_error_message:

            ;// print line-break
                CALL CRLF;

            ;// save starting position of the user's white-space trimmed input for later
                PUSH edx;

            ;// Print: Unable to interpret "
                LEA edx, err_msg_part_1;
                CALL WRITESTRING; (string start edx)

            ;// restore to edx the starting position of the trimmed user input 
                POP edx;

            ;// print user's input
                CALL WRITESTRING; (string start edx)

            ;// Print: " as a positive integer.
                LEA edx, err_msg_part_2;
                CALL WRITESTRING; (string start edx)

        ;// clear zero flag (indicates invalid input recieved)
            PUSH eax;
            OR al, 1;
            POP eax;

        ;// jump to done.
            JMP done;

    valid_input_recieved:
        ;// set zero flag
            TEST al, 0;

        ;// return final value in ecx, not eax
            MOV ecx, eax;
    done:

        POP edx;
        POP eax;

        ; // 'RET' will be automatically get repaced 
        ; // with 'LEAVE' and 'RET'
        ; // because there are paramaters apearing after
        ; // "USER_INT PROC"
 
        RET; // return

USER_INT ENDP; // end procedure named "USER_INT"

COMMENT /************************************************************
        IS_NUMERIC_STRING

        Description:      Checks whether or not a string contains only
                          characters coresponding to ASCII digits 0, 1,
                          ..., 8, 9. Ignores leading and trailing
                          white-space.

        Preconditions:    STACK:     Top of stack is a DWORD
                                     containing the memory
                                     offset of a null-terminated
                                     string.

        Postconditions:   
                          STACK:     Top 1 DWORD has been popped off
                                     of the stack.

                          zero flag: Set if string starting at memory
                                     offset stored in edx contains only
                                     characters coresponding to ASCII
                                     digits 0, 1,..., 8, 9, ignoring
                                     leading and trailing white-space.
                                     zero flag is clear otherwise.

                          EDX:       Contains memory offset of the
                                     first non-white space character
                                     in the original string.
                                     
                                     Last white space character
                                     encountered when traversing
                                     from end of string towards its
                                     beggining is replaced with null.

                                     "white-space" chars are defined as
                                     follows:

                                     0Ah    line feed
                                     0Dh    Carriage return
                                     20h    space character

*********************************************************************/
IS_NUMERIC_STRING PROC, string:DWORD;
    
    ;//  
        PUSH eax;
    ;//
        MOV edx, string;

    ;//  remove white space from start and end
    ;// edx <-
        INVOKE TRIM, edx

    PUSH edx;

    while_234: ;// while current character is a numeral
    MOV al, [edx]
         ;// zero_flag <-
             CALL ISDIGIT; (AL) ;// ISDIGIT is from the Irvine library
         ;// if zero_flag == 0, JMP not_digit
         JNZ not_digit;
         INC edx;
    JMP while_234;

    not_digit:
        ;// if AL == null, jump to "is_null"
            CMP al, 0;
            JZ  is_null;
        ;// At this point, AL is not a digit or null.
        ;// clear zero flag. string is not numeric
            PUSH eax;
            OR al, 1;
            POP eax;
            JMP done;
    is_null:
        ;// set zero_flag
        TEST al, 0;
    done:
             
    POP edx;
    POP eax;
    RET; // return

IS_NUMERIC_STRING ENDP; // end procedure named "IS_NUMERIC_STRING"  

COMMENT /************************************************************
        TRIM

        Description:      Removes white space characters
                          from the beggining and end of
                          a null-terminated string

        Preconditions:  STACK: On top of the stack is a DWORD which
                               contains the memory offset of a
                               null-terminated string

        Postconditions: STACK: string offset has been removed from
                               the top of the stack.

                          EDX: Contains memory offset of the
                               first non-white space character
                               in the original string.

                               The last white-space character
                               encountered while traversing
                               from end of string towards its
                               beggining is replaced with null.

                               "white-space" chars are defined as
                               follows:

                               0Ah    line feed
                               0Dh    Carriage return
                               20h    space character

*********************************************************************/
TRIM PROC, string:DWORD;

    PUSH eax;
    PUSH esi;

    MOV edx, string;

    traverse_untill_not_white:
        ;// if zero_flag clear
            MOV al, [edx]
            ;// zero_flag = 
                INVOKE IS_WHITE, eax
        ;// if zero_flag clear then JMP first_non_white
            JNZ first_non_white_encountered;
        ;// if zero_flag == 1 then advance pointer one byte forward
            INC edx;
    JMP traverse_untill_not_white;

    first_non_white_encountered:
        ;// if [edx] is null then JMP done
            CMP BYTE PTR [edx], 0;
            JZ done;
        ;// if [edx] is not null or a white space character, then
        ;// record its position and advance pointer one byte forward
            PUSH edx;
            INC edx;

    traverse_untill_white_or_null:
        ;// chech whether or not current character is a white-space char
            MOV al, [edx]
            ;// zero_flag =
                INVOKE IS_WHITE, eax
        ;// if character is not white, then skip the following
            JNZ keep_going;
        ;// if character is white then log its position and jump to traverse_untill_not_white2
            MOV esi, edx;
            INC edx;
            JMP traverse_untill_not_white2;
            keep_going:
        ;// if character is null, JMP fix_start
            CMP al, 0;
            JZ fix_start;
        INC edx; // advance pointer one byte forward
    JMP traverse_untill_white_or_null;

    traverse_untill_not_white2:
             MOV al, [edx]
             ;// zero_flag <-
                 INVOKE IS_WHITE, eax
         ;// if character is not white jump to non_white
             JNZ non_white_encountered;
         ;// if character is white (zero_flag == 1)
         ;// then advance pointer one byte forward
             INC edx;
    JMP traverse_untill_not_white2;

    non_white_encountered:
        ;// if [edx] is null then JMP fix_end
            CMP BYTE PTR [edx], 0;
            JZ fix_end;
        ;// if [edx] is not null or a white space character, then
        ;// advance pointer one byte forward and jump to
        ;// traverse_untill_white_or_null
            INC edx;
            JMP traverse_untill_white_or_null;

    fix_end:
        MOV (BYTE PTR [esi]), 0;
    fix_start:
        POP edx;

    done:
        POP esi;        
        POP eax;
        RET; // return

TRIM ENDP; // end procedure named "TRIM"

COMMENT /************************************************************
        IS_WHITE

        Description: Tests a character to see whether it is
                     a white-space character or not.

        Preconditions:    Top of the stack contains a DWORD
                          the least significant BYTE of which
                          is a character. The other 3 bytes of
                          the dword are ignored.

        Postconditions:   zero flag is set if the character is
                          a white space character. zero flag is
                          clear otherwise.

                          "white-space" chars are defined as
                           follows:

                           0Ah    line feed
                           0Dh    Carriage return
                           20h    space character

                           Character has been removed from the top
                           of the stack. Stack pointer has been
                           moved.
*********************************************************************/
IS_WHITE PROC, character:DWORD;

    line_feed = 0Ah;
    car_ret   = 0Dh;
    space     = 20h;

    ;//
        PUSH eax;
        MOV eax, character;

    ;// if AL == line_feed return 1
        CMP al, line_feed
        JZ set_zero_flag;

    ;// if AL == car_ret return 1
        CMP al, car_ret
        JZ set_zero_flag;

    ;// if AL == space return 1
        CMP al, space
        JZ set_zero_flag;

    JMP clear_zero_flag;

    set_zero_flag:
        TEST al, 0;
        JMP done;

    clear_zero_flag:
        PUSH eax;
        OR al, 1;
        POP eax;

    done:
        POP eax;
        RET; 

IS_WHITE ENDP; // end procedure

COMMENT /************************************************************
        PRINT_MENU_AND_GET_SELECTION

        Description:      Prints a menu out to console and returns
                          user's selection.                 

        Preconditions:    STACK: top DWORD of stack contains memory
                                 offset of a null-terminated array
                                 of other DWORDS.
                                 Each DWORD is to be the memory
                                 offset of the location of the
                                 first character of a null-terminated
                                 string. [ESI] is to be memory
                                 offset the menu's name or title.
                                 for i >= 1,[ESI + i*4] is to be
                                 mem offset of the ith menu option

        Postconditions:   STACK: topmost DWORD ahs been popped.

                          ECX: Contains user's selection:

                               1 if user chose option pointed
                                 to by [esi + 4]

                               2 if user chose option pointed
                                 to by [esi + 8]

                               3 if user chose option pointed
                                 to by the fourth element of the
                                 array pointed to by esi

                                 etc...

*********************************************************************/
PRINT_MENU_AND_GET_SELECTION PROC, menu_opts:DWORD; // begin procedure 

    ;// Reserve memory on the stack for local variables
        str_empty_nbytes = 1;
        str_dot_space_nbytes = 3;
        byte_total = str_empty_nbytes + str_dot_space_nbytes;

        ;// want to end on a double word boundary

        ;// CEILING(a/b) ==  1 + FLOOR((a-1)/b)

        dword_count = 1 + ( (byte_total - 1) / 4);
        SUB esp, 4*dword_count;

        str_empty     EQU [ebp - str_empty_nbytes];
        str_dot_space EQU [ebp - (str_empty_nbytes + str_dot_space_nbytes)];

    ;//
        PUSH eax;
        PUSH ebx;
        PUSH edx;
        PUSH esi;

    ; // str_empty BYTE 0;
        LEA edx, str_empty;
        MOV BYTE PTR[edx], 0;

    ; // str_dot_space  BYTE '. ', 0
        LEA edx, str_dot_space;
        MOV BYTE PTR[edx], '.';
        INC edx;
        MOV BYTE PTR[edx], ' ';
        INC edx;
        MOV BYTE PTR[edx], 0;
    
    ;//
        MOV esi, menu_opts;

    ;// get title length
        MOV edx, DWORD PTR[esi];
        ;// ecx <-
            INVOKE STR_LEN, edx;

    ;// print_menu_title followed by a horizontal bar
        INVOKE PRINT_HEADER, edx, ecx;

    ;// print_menu_options:

        ;// Advance ESI from pointing to menu title
        ;// to pointing to first menu option
            ADD esi, 4;

        ;// Initialize index of menu option
            MOV eax, 1;        

        loop_1_start:
            ;// if [esi] == 0, break from loop
                CMP DWORD PTR [esi], 0
                JZ loop_1_end;

            ;// Print line-break
                CALL CRLF;

            ;// Print option number
                CALL WRITEDEC; (int eax)

            ;// Print ". "
                LEA edx, str_dot_space;
                CALL WRITESTRING; (string begining edx)

            ;// Print menu option
                MOV edx, DWORD PTR [esi];
                CALL WRITESTRING; (string begining edx)

            ;// Advance pointer to next menu-option and increment option number
                INC eax;
                ADD esi, 4; 

            JMP loop_1_start;
        loop_1_end:

        ;// loop incremented eax one-too-many times.
        ;// The last iteration of the loop incremented eax in preperation for a
        ;// future loop iteration, but there was no next loop iteration. eax is
        ;// currently a number for a non-existant "next" menu option.
        ;// Decrement eax to make eax correspond to the last existing menu option

            DEC eax;
   
    ;// Get menu selection from user
        MOV ebx, 1; // min valid menu selection
        LEA edx, str_empty;
        ;// ecx <-
            INVOKE USER_INT_RANGE_LOOPED, ebx, eax, edx

    POP esi;
    POP edx;
    POP ebx;
    POP eax;
    RET;

PRINT_MENU_AND_GET_SELECTION ENDP; // end procedure

COMMENT /************************************************************
        STR_LEN

        Description:      Returns the length of a null-terminated
                          string. Excludes the terminating-null
                          from the count.

        Preconditions:    STACK: top if stack si a DWORD containing
                                 the memory offset of a null-terminated
                                 string

        Postconditions:   ECX: contains number of elements in
                               the string excluding the terminating
                               null.

                          STACK: top DWORD of stack haas been popped

*********************************************************************/
STR_LEN PROC, string:DWORD;  // begin procedure

    PUSH edx;

    MOV edx, string;

    ;// initialize accumulator
        MOV ecx, 0;
    loop_check_for_null:
        ;// if [edx] is null JMP quit 
            CMP BYTE PTR [edx], 0
            JZ quit;
        ;// increment character count
            INC ecx;
        ;// Advance pointer one byte forward
            INC edx;
    JMP loop_check_for_null;
    
    quit:
        POP edx;
        RET; // return

STR_LEN ENDP; // end procedure

COMMENT /************************************************************
        PRINT_HEADER

        Description:   Prints a string followed bya horizontal bar
                       (long string of concecutive hyphen characters)
                       For example, we might print something like the
                       following:

                       BLAH BLAH BLAH
                       --------------------------------------------

        Preconditions:    STACK: top of stack has the following
                                 on it:

                offset_of_str: Contains memory offset pointing
                               to the first character in a
                               null-terminated string

                     quantity: Desired length of horizontal bar
                               (desired number of hyphen chars),
                               unless value is zero. If input is
                               zero, a very very long horizontal
                               bar will be printed
   

        Postconditions:   STACK: top two DWORDs were removed.

                          CONSOLE:String pointed to by offset_of_str
                          was printed to the console, followed by a
                          horizontal bar
                          of the length (character count) specified
                          in quantity.

*********************************************************************/
PRINT_HEADER PROC, offset_of_str:DWORD, quantity:DWORD;

    ;//  
        PUSH eax;
        PUSH ecx;
        PUSH edx;

    ;//
        MOV edx, offset_of_str;
        MOV ecx, quantity;

    ;// begin printing at begining of a new-line  
        CALL CRLF;     
 
    ;//  
        CALL WRITESTRING; (string edx)

    ;// begin printing horizontal bar at begining of a new-line 
        CALL CRLF;

    ;// 
        MOV al,'-';
        INVOKE REPEAT_CHAR, eax, ecx;

    ;//  
        POP edx;
        POP ecx;
        POP eax;

    ;// Return
        RET; 

PRINT_HEADER ENDP; // end procedure

COMMENT /************************************************************
        REPEAT_CHAR

        Description:      Prints a sequence of identicle characters
                          to the console. For example, if the
                          character is a hyphen, then we might print
                          something like the following:

                          ---------------------------------------

        Preconditions:    STACK: top of the stack has on it:

                                     a DWORD the least significant
                                     BYTE of which is an ASCII
                                     character code. The other 3 bytes
                                     of the dword are ignored.

                                     a DWORD containing the number
                                     copies desired

        Postconditions:   CONSOLE: ECX copies of the character
                                   were printed to the console.

*********************************************************************/
REPEAT_CHAR PROC, character:DWORD, quantity:DWORD;  // begin procedure 

    PUSH eax;
    PUSH ecx;
    MOV eax, character
    MOV ecx, quantity;

        L:
            CALL WRITECHAR;(char al)
        LOOP L;

    POP ecx;
    POP eax;
    RET;

REPEAT_CHAR ENDP; // end procedure

END main;