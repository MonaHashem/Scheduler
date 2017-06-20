event_in_course(csen403, labquiz1, assignment).
event_in_course(csen403, labquiz2, assignment).
event_in_course(csen403, project1, evaluation).
event_in_course(csen403, project2, evaluation).
event_in_course(csen403, quiz1, quiz).
event_in_course(csen403, quiz2, quiz).
event_in_course(csen403, quiz3, quiz).

event_in_course(csen401, quiz1, quiz).
event_in_course(csen401, quiz2, quiz).
event_in_course(csen401, quiz3, quiz).
event_in_course(csen401, milestone1, evaluation).
event_in_course(csen401, milestone2, evaluation).
event_in_course(csen401, milestone3, evaluation).

event_in_course(csen402, quiz1, quiz).
event_in_course(csen402, quiz2, quiz).
event_in_course(csen402, quiz3, quiz).

event_in_course(math401, quiz1, quiz).
event_in_course(math401, quiz2, quiz).
event_in_course(math401, quiz3, quiz).

event_in_course(elct401, quiz1, quiz).
event_in_course(elct401, quiz2, quiz).
event_in_course(elct401, quiz3, quiz).
event_in_course(elct401, assignment1, assignment).
event_in_course(elct401, assignment2, assignment).

event_in_course(csen601, quiz1, quiz).
event_in_course(csen601, quiz2, quiz).
event_in_course(csen601, quiz3, quiz).
event_in_course(csen601, project, evaluation).
event_in_course(csen603, quiz1, quiz).
event_in_course(csen603, quiz2, quiz).
event_in_course(csen603, quiz3, quiz).

event_in_course(csen602, quiz1, quiz).
event_in_course(csen602, quiz2, quiz).
event_in_course(csen602, quiz3, quiz).

event_in_course(csen604, quiz1, quiz).
event_in_course(csen604, quiz2, quiz).
event_in_course(csen604, quiz3, quiz).
event_in_course(csen604, project1, evaluation).
event_in_course(csen604, project2, evaluation).


holiday(3,monday).
holiday(5,tuesday).
holiday(10,sunday).


studying(csen403, group4MET).
studying(csen401, group4MET).
studying(csen402, group4MET).

studying(csen601, group6MET).
studying(csen602, group6MET).
studying(csen603, group6MET).
studying(csen604, group6MET).

should_precede(csen403,project1,project2).
should_precede(csen403,quiz1,quiz2).
should_precede(csen403,quiz2,quiz3).

quizslot(group4MET, tuesday, 1).
quizslot(group4MET, thursday, 1).
quizslot(group6MET, saturday, 5).

% ------------------------------------------------------------------


% The representation that we have chosen for our Schedule is as follows:
% We have a main schedule consisting of all of the individual schedules for each group
% The design of a group's schedule is as follows:
% a set "events" as shown --> event(Week Number, The event's details, The allocated Slot)
% A sample of the output is shown below.

% SAMPLE OUTPUT:-

%[group_schedule(group4MET,[event(1,event_in_course(csen401,milestone1,evaluation),quizslot(thursday,1)),event(1,event_in_course(csen401,milestone2,evaluation),quizslot(tuesday,1))
%,event(2,event_in_course(csen401,milestone3,evaluation),quizslot(thursday,1)),
% event(2,event_in_course(csen401,quiz1,quiz),quizslot(tuesday,1)),
% event(4,event_in_course(csen401,quiz2,quiz),quizslot(thursday,1)),
% event(6,event_in_course(csen401,quiz3,quiz),quizslot(thursday,1)),
% event(3,event_in_course(csen402,quiz1,quiz),quizslot(thursday,1)),
% event(5,event_in_course(csen402,quiz2,quiz),quizslot(thursday,1)),
% event(7,event_in_course(csen402,quiz3,quiz),quizslot(thursday,1)),
% event(3,event_in_course(csen403,labquiz1,assignment),quizslot(tuesday,1)),
% event(4,event_in_course(csen403,labquiz2,assignment),quizslot(tuesday,1)),
% event(6,event_in_course(csen403,project1,evaluation),quizslot(tuesday,1)),
% event(7,event_in_course(csen403,project2,evaluation),quizslot(tuesday,1)),
% event(8,event_in_course(csen403,quiz1,quiz),quizslot(thursday,1)),
% event(10,event_in_course(csen403,quiz2,quiz),quizslot(thursday,1)),
% event(12,event_in_course(csen403,quiz3,quiz),quizslot(thursday,1))]),
% group_schedule(group6MET,[event(1,event_in_course(csen601,project,evaluation),quizslot(saturday,5)),
% event(2,event_in_course(csen601,quiz1,quiz),quizslot(saturday,5)),
% event(4,event_in_course(csen601,quiz2,quiz),quizslot(saturday,5)),
% event(6,event_in_course(csen601,quiz3,quiz),quizslot(saturday,5)),
% event(3,event_in_course(csen602,quiz1,quiz),quizslot(saturday,5)),
% event(5,event_in_course(csen602,quiz2,quiz),quizslot(saturday,5)),
% event(7,event_in_course(csen602,quiz3,quiz),quizslot(saturday,5)),
% event(8,event_in_course(csen603,quiz1,quiz),quizslot(saturday,5)),
% event(10,event_in_course(csen603,quiz2,quiz),quizslot(saturday,5)),
% event(12,event_in_course(csen603,quiz3,quiz),quizslot(saturday,5)),
% event(9,event_in_course(csen604,project1,evaluation),quizslot(saturday,5)),
% event(14,event_in_course(csen604,project2,evaluation),quizslot(saturday,5)),
% event(11,event_in_course(csen604,quiz1,quiz),quizslot(saturday,5)),
% event(13,event_in_course(csen604,quiz2,quiz),quizslot(saturday,5)),
% event(15,event_in_course(csen604,quiz3,quiz),quizslot(saturday,5))])]


%------------------------------------------------------------------------------

% PREDICATES FOR THE SCHEDULE:-

% [1] the predicate searches for the respective group's schedule in the main schedule and passes
%it to precede2

precede(GroupName,Schedule):-
		get_groupschedule(GroupName,Schedule,SC),
		precede2(SC).


% [1.1] the precede2 predicate:
% it takes as a paramter the schedule for the designated group and checks
% that for every event that has a pair in should_precede(event1,event2)
% it checks whether or not the event2 exists in the schedule after event1

precede2(Schedule):-
		precede2_helper(Schedule,Schedule).


% [1.2] precede2_helper takes initially as paramters two copies of a group's schedule
% and one by one checks that should the events in the first copy have a matching pair
% in should_precede it verifies that its pair (in copy2) satisfies the necessary constraints

precede2_helper([],_).
precede2_helper([H|T],L):-  H=event(_,event_in_course(Course,Event,_),_) , \+should_precede(Course,Event,_), precede2_helper(T,L).
precede2_helper([H|T],L):-  H=event(_,event_in_course(Course,Event,_),_),
							should_precede(Course,Event,SecondEvent),
							\+member(event(_,event_in_course(Course,SecondEvent,_),_),L),
							precede2_helper(T,L).

precede2_helper([H|T],L):-  H=event(Week,event_in_course(Course,Event,_),Time),
							should_precede(Course,Event,SecondEvent),
							member(event(Week1,event_in_course(Course,SecondEvent,_),Time1),L),
							time_check([Time,Time1],Week,Week1), precede2_helper(T,L).

% [1.3] it verifies that either the date of the second events
% exceeds that of the first by at least one week
% or if they are the same it passes it on to check order predicate else it returns false

time_check(Timings,WeekEvent1,WeekEvent2):-
			(WeekEvent1< WeekEvent2 ); (WeekEvent1= WeekEvent2 ,check_order(Timings)).

%[1.4] it checks the order of two slots if they are on the same day then check that slot1 is before slot2
% else if they are not it verifies that day1 exists before day2

check_order([H,H1|T]):- H= quizslot(D,S1), H1=quizslot(D1,S2), D=D1, S1<S2, check_order(T).
check_order([H,H1|T]):- H= quizslot(D,_), H1=quizslot(D1,_), D\=D1, days(L), find(D,L,L1), member(D1,L1), check_order(T).
check_order([_]).


%[1.5] returns list of days after day H used as a helper method to find the ordered day

find(H,[H|T],T).
find(H,[H],[]).
find(H,[H1|T],T1):- H\=H1, find(H,T,T1).

%[8.0] it passes the designated group G's schedule from get_groupschedule to the
% no_holidays2 to verify that it satisfies the constraints

no_holidays(G,Schedule):-
	get_groupschedule(G,Schedule,SC),
	no_holidays2(SC).


%[8.1] the no_holidays2 predicate : should succeed only if Schedule has no events scheduled
% in any of the available holidays

no_holidays2(Schedule):-
	listOfHolidays(H), no_holidays2_helper(H,Schedule).

%[8.2] a helper predicate of the no_holidays2 that checks that none of the events' quizslots
% overlap with the stated holidays

no_holidays2_helper([],_).
no_holidays2_helper([H|T], Schedule):- H=holiday(W,D), E=event(W,_,quizslot(D,_)),
					\+member(E,Schedule), no_holidays2_helper(T,Schedule).

%[8.3] It generates a list of all holidays

listOfHolidays(Holidays):- setof(holiday(W,D),holiday(W,D),Holidays).





% all_groups and groups are predicates that generates a list containing the groups.

all_groups(G):- setof(Group,studying(_,Group),G).

groups(G):- setof(Group,all_groups(Group),Gs), flatten(Gs,G).


% a predicate that generates a list consisting of the week numbers according to the desired
% number of weeks.

weekNum(Num,[Num],Num).
weekNum(NW,[Acc|L3],Acc):- A is Acc +1, weekNum(NW,L3,A).

% schedule_helper is the predicate that generates the main schedule. It takes as parameters
% the number of weeks in the semester, the main schedule and the list of groups. 
% it gets the available_timings, weekNum and group_events to send them to the main_helper 
% to generate the schedule for each group  

schedule_helper(_,[],[]).
schedule_helper(Week_Num,Schedule,[GroupName|Groups]):-
Schedule = [group_schedule(GroupName,GroupSchedule)|T],
group_events(GroupName, E),
available_timings(GroupName,Time),
weekNum(Week_Num,List,1),
main_helper(E,Time,[],GroupSchedule,List),
schedule_helper(Week_Num, T,Groups).

% schedule is the main predicate the generates the whole main schedule.

schedule(Week_Number,Schedule):-
groups(Groups),
schedule_helper(Week_Number,Schedule,Groups).



% main_helper is the main predicate that generates the schedule of the group, 
% it takes as parameters a list of events for the group, a list of available slots,
% the schedule so far, the generated schedule, and a list containing the week numbers
% it takes each event and takes a member of the available slots and a week and tries to 
% to add it to the schedule by checking the constraints.  

main_helper([],_,S,S,_).
main_helper([E|Event],Time,Schedule_sofar,Out,WeekNum):-
			member(Week,WeekNum), member(T,Time),  GS=[event(Week,E,T)],
			append(Schedule_sofar,GS,Updated_Schedule),
			precede2(Updated_Schedule),
			no_same_day_quiz2(Updated_Schedule),
			no_consec_quizzes2(Updated_Schedule),
			valid_slots_schedule2(Updated_Schedule),
			no_same_day_assignment2(Updated_Schedule),
			no_holidays2(Updated_Schedule),

main_helper(Event,Time,Updated_Schedule,Out, WeekNum).



% [4.0] is a flattened list of events that should be scheduled for the group G.
group_events(G,Events):-
      setof(B,events_helper2(G,B),Event), flatten(Event,Events).

% [4.1] this predicate generates the list of events of a course taken by a group.

events_helper2(Group,B):- studying(Course,Group), events_helper3(Course,B).


% [4.2] this predicate generates list of events of a specific course

events_helper3(C,B):- setof(event_in_course(C,X,X1),event_in_course(C,X,X1),B).





% [3.0] simplified inordered version of available_timings

possible_slots(Group, S):- setof(quizslot(D,Slot_num),quizslot(Group,D,Slot_num),S).



% [3.1] predicate returning an ordered list of available_timings for a group check out the 
% new structure of quizslot available_timings(G,List):- possible_slots(G,L1),my_sort(L1,List).
% insertion sort 

available_timings(G,List):- possible_slots(G,L1),my_sort(L1,List).



my_sort([],[]).
my_sort([quizslot(Day1,Slot1)|Rest],Out):-
my_sort(Rest,RestSorted),
my_insert(quizslot(Day1,Slot1),RestSorted,Out).

my_insert(X,[],[X]).

my_insert(quizslot(Day1,Slot1),[quizslot(Day2,Slot2)|Rest],[quizslot(Day1,Slot1),quizslot(Day2,Slot2)|Rest]):-
  Day1\=Day2,days(L), nth0(Index1,L,Day1),nth0(Index2,L,Day2), Index1<Index2.

my_insert(quizslot(Day,Slot1),[quizslot(Day,Slot2)|Rest],[quizslot(Day,Slot1),quizslot(Day,Slot2)|Rest]):-
  Slot1<Slot2.

my_insert(quizslot(Day1,Slot1),[quizslot(Day2,Slot2)|Rest],[quizslot(Day2,Slot2)|Rest2]):-
    Day1\=Day2,days(L), nth0(Index1,L,Day1),nth0(Index2,L,Day2),
Index1>Index2,
my_insert(quizslot(Day1,Slot1),Rest,Rest2).

my_insert(quizslot(Day,Slot1),[quizslot(Day,Slot2)|Rest],[quizslot(Day,Slot2)|Rest2]):-
  Slot1>Slot2,
my_insert(quizslot(Day,Slot1),Rest,Rest2).

% days of the week
days(L):-L=[saturday,sunday,monday,tuesday,wednesday,thursday].

% a predicate to get the schedule of the specific group from the whole
% schedule

get_groupschedule(G,[group_schedule(GroupName,GroupSchedule)|_],GS):-
		G=GroupName,GS= GroupSchedule.
get_groupschedule(G,[group_schedule(GroupName,_)|T],GS):-
		G\=GroupName, get_groupschedule(G,T,GS).

%[5.0] This predicate searches for the Group G's schedule in main "Schedule"
% then passes it on to no_consec_quizzes2

no_consec_quizzes(G,Schedule):-
	get_groupschedule(G,Schedule,GS),
	no_consec_quizzes2(GS).


%[5.1] This predicate checks that there is not more than one quiz per course per week (using the 1st member predicate below)
% as well as that there would be no quizzes for the same course in the week before
% and the week after (the 2nd and 3rd member predicates below)

no_consec_quizzes2([]).
no_consec_quizzes2([event(W1,E1,_)|T]):-
   E1 = event_in_course(C,_,quiz),
   W2 is W1+1,
   W3 is W1 -1,
   \+ member(event(W1,event_in_course(C,_,quiz),_),T),
   \+ member(event(W2,event_in_course(C,_,quiz),_),T),
   \+ member(event(W3,event_in_course(C,_,quiz),_),T),
   no_consec_quizzes2(T).

% in case event E1 is not of type quiz it ignores it and carries on
no_consec_quizzes2([event(_,E1,_)|T]):-
   E1 \= event_in_course(_,_,quiz),
  no_consec_quizzes2(T).

%[6.0] it searches for the Group G's schedule and passes it on to no_same_day_quiz2

no_same_day_quiz(G,Schedule):-
				get_groupschedule(G,Schedule,GS),
				no_same_day_quiz2(GS).

%[6.1] it verifies that there is only one possible quiz event
% in all of the courses per day

no_same_day_quiz2([]).
no_same_day_quiz2([event(W1,E1,Q1)|T]):-
    E1 = event_in_course(_,_,quiz),
    Q1 = quizslot(D,_),
    \+ member(event(W1,event_in_course(_,_,quiz),quizslot(D,_)),T),
    no_same_day_quiz2(T).

% if the event E1 is not a quiz it ignores it and carries on

no_same_day_quiz2([event(_,E1,_)|T]):-
    E1 \= event_in_course(_,_,quiz),
    no_same_day_quiz2(T).



%[7.0] it searches for the Group G's schedule and passes it on to no_same_day_assignment2

no_same_day_assignment(G,Schedule):-
get_groupschedule(G,Schedule,GS),
		no_same_day_assignment2(GS).

%[7.1] it verifies that there is only one possible assignment event
% in all of the courses per day

no_same_day_assignment2([]).
no_same_day_assignment2([event(W1,E1,Q1)|T]):-
    E1 = event_in_course(_,_,assignment),
    Q1 = quizslot(D,_),
    \+ member(event(W1,event_in_course(_,_,assignment),quizslot(D,_)),T),
    no_same_day_assignment2(T).


% if the event E1 is not a quiz it ignores it and carries on
	
no_same_day_assignment2([event(_,E1,_)|T]):-
    E1 \= event_in_course(_,_,assignment),
    no_same_day_assignment2(T).




% a predicate that checks that the Group G doesn't have any conflict
% between events.

valid_slots_schedule(G,Schedule):-
	get_groupschedule(G,Schedule,GS),
	valid_slots_schedule2(GS).

% valid_slots_schedule2 is a predicate that checks that the schedule of the group
% does not have any conflict between events.

valid_slots_schedule2([]).

valid_slots_schedule2([event(WeekNumber,_,QS)|T]):-
			\+member(event(WeekNumber,_,QS),T),
			valid_slots_schedule2(T).
