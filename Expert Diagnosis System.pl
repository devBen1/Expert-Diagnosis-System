%To Start the system type start.
% Name : - devBen

:- use_module(library(jpl)).
start :-sleep(0.4),	
		write('-----------------------------------------------------------------'),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("###################||| EXPERT DIAGNOSIS SYSTEM |||#########################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write('-----------------------------------------------------------------'),nl,nl,nl,
		
		
        /*write("Hi. How are you? First of all tell me your name Please : "),
        read(Patient),*/
		
		
		interface2.
		
		
       /* hypothesis(Patient,Disease),
        write(Patient),write(', you '), write(' probably have '),write(Disease),write('.'),undo,
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.*/
        
        
    symptom(Patient,fever) :- verify(Patient," have a fever (y/n) ?").
 
    symptom(Patient,headache) :- verify(Patient," have a headache (y/n) ?").
 
    symptom(Patient,abdominal_pain) :- verify(Patient," have abdominal pain (y/n) ?").
 
    symptom(Patient,profuse_diarrhea) :- verify(Patient," have profuse diarrhea (y/n) ?").
  
    symptom(Patient,shaking_chills) :- verify(Patient," have shaking chills (y/n) ?").

    symptom(Patient,sweating_profusely) :- verify(Patient," sweating profusely (y/n) ?").
    
    symptom(Patient,coughing) :- verify(Patient," have cough (y/n) ?").
    
    symptom(Patient,poor_appetite) :- verify(Patient," have poor appetite (y/n) ?").
    
    symptom(Patient,blood_in_stool) :- verify(Patient," have blood in your stool (y/n) ?").
	
    symptom(Patient,general_aches) :- verify(Patient," have general aches and pains (y/n) ?").
  
    symptom(Patient,severe_thirst) :- verify(Patient," have servere thrist (y/n) ?").
  
    symptom(Patient,watery_stool) :- verify(Patient," have watery stool (y/n) ?").
   
    symptom(Patient,muscle_cramp) :- verify(Patient," have muscle cramp (y/n) ?").
   
    symptom(Patient,need_to_have_bowl_movement) :- verify(Patient," have a urgent need to have a bowl movement (y/n) ?").
    
    symptom(Patient,mucus_in_stool) :- verify(Patient," have mucus in your stool (y/n) ?").
   
    symptom(Patient,fatigue) :- verify(Patient," feel tired (y/n) ?").
	
    symptom(Patient,bloating) :- verify(Patient," have bloating (y/n) ?").
   
    symptom(Patient,need_to_vomit) :- verify(Patient," feel like vomitting (y/n) ?").

    symptom(Patient,dehydrated_feeling) :- verify(Patient," feel dehydrated (y/n) ?").
	
	/*symptom(_,"Sorry, your disease is not in my knowledge domain.").*/   

        
    hypothesis(Patient,diarrhea) :-
        symptom(Patient,abdominal_pain),
        symptom(Patient,watery_stool),
        symptom(Patient,fever),
        symptom(Patient,need_to_have_bowl_movement),
        symptom(Patient,dehydrated_feeling),
        symptom(Patient,blood_in_stool),
        symptom(Patient,mucus_in_stool),
        symptom(Patient,bloating). 

    hypothesis(Patient,cholera) :-
        symptom(Patient,profuse_diarrhea),
        symptom(Patient,severe_thirst),
        symptom(Patient,need_to_vomit),
        symptom(Patient,abdominal_pain),
        symptom(Patient,muscle_cramp).
    
    hypothesis(Patient,typhoid) :-
        symptom(Patient,fever),
        symptom(Patient,headache),
        symptom(Patient,poor_appetite),
        symptom(Patient,general_aches),
        symptom(Patient,abdominal_pain),
        symptom(Patient,fatigue).
        
    hypothesis(Patient,malaria) :-
        symptom(Patient,fever),
        symptom(Patient,headache),
        symptom(Patient,shaking_chills),
        symptom(Patient,sweating_profusely),
        symptom(Patient,coughing).
        
	hypothesis(_,"a disease. But I'm Sorry, your disease is not in my knowledge domain.").
	
    response(Reply) :-
        read(Reply),
        write(Reply),nl.
		
ask(Patient,Question) :-
	write(Patient),write(', do you'),write(Question),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),*/
	
	interface(', do you',Patient,Question),
	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.
	
:- dynamic yes/1,no/1.		
	
verify(P,S) :-
   (yes(S) 
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(P,S))).
	 
undo :- retract(yes(_)),fail. 
undo :- retract(no(_)),fail.
undo.


pt(Patient):- 

		hypothesis(Patient,Disease),
		interface3(Patient,', you probably have ',Disease,'.'),
        write(Patient),write(', you probably have '),write(Disease),write('.'),undo,end.

end :-
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USING ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Expert Diagnosis System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT DIAGNOSIS SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [200,50], _),
	jpl_call(F, setSize, [800,700], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _), 
	write(N),nl,
	( (N == yes ; N == y)
      ->
       assert(yes(Z)) ;
       assert(no(Z)), fail).
	   		
interface2 :-
	jpl_new('javax.swing.JFrame', ['Expert Diagnosis System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT DIAGNOSIS SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [200,50], _),
	jpl_call(F, setSize, [800,700], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi. How are you? First of all tell me your name please'], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(null)
		->	write('You cancelled'),interface3('You cancelled. ','Thank you ','for using ','me.'),end,fail
		;	write("Hi. How are you? First of all tell me your name please : "),write(N),nl,pt(N)
	).
	
	
interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Expert Diagnosis System'], F),
	jpl_new('javax.swing.JLabel',['--- MEDICAL EXPERT DIAGNOSIS SYSTEM ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [200,50], _),
	jpl_call(F, setSize, [800,700], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _), 
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).
	
help :- write("To start the diagnosis system please type 'start.' and press Enter key").
