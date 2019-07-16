var trials = [];
var trialindex = 0;
var stim_per_condition = 25;//x3 conditions, gram, acc, sense. Est time per cond?

function responseListener(aresponse){//global so it'll be just sitting here available for the trial objects to use. So, it must accept whatever they're passing.
    //    console.log("responseListener heard: "+aresponse); //diag
    trials[trialindex].response = aresponse;
    trials[trialindex].responseTime= Date.now();

    var saveme = JSON.stringify(trials[trialindex])
    
    $.post('/response',{myresponse:saveme},function(success){
    	console.log(success);//For now server returns the string "success" for success, otherwise error message.
    });
    console.log(saveme); //note all trial types send responses here: acc, gram and spacer.
    
    //can put this inside the success callback, if the next trial depends on some server-side info.
    trialindex++; //increment index here at the last possible minute before drawing the next trial, so trials[trialindex] always refers to the current trial.
    nextTrial();
}

function nextTrial(){
    if(trialindex<trials.length){
	trials[trialindex].drawMe("uberdiv");
	setTimeout(function(){Array.from(document.getElementsByClassName('responsebutton'),function(x){x.disabled=false})},
		   1000);
    }else{
	$.post("/finish",function(data){window.location.replace(data)});
    }
}

// a trial object should have a drawMe function and a bunch of attributes.
//the data-getting process in 'dashboard.ejs' & getData routes creates a csv with a col for every attribute, using 'Object.keys' to list all the properties of the object. Assumes a pattern where everything interesting is saved to the trial object, then that is JSONified and saved as a response.
//Note functions are dropped by JSON.
//Also note this means you have to be consistent with the things that are added to each trial before they are saved, maybe init with NA values in the constructor.
function makeTrial(questiontext,targetsentence, hm_options,option_bookend_labels){
    this.ppntID = localStorage.getItem("ppntID");
    this.questiontext = questiontext;
    this.text = targetsentence;
    this.hm_options=hm_options;
    this.bookend_labels=option_bookend_labels;

    this.drawMe = function(targdiv){
	this.drawTime = Date.now();
	var drawstring = "<h2>"+this.questiontext+"</h2>"+"<p class='centered' id='targtext'>"+targetsentence+"</p>"+
	    "<table class='centered' width='70%'><tr>"
		for(var i =0;i<hm_options;i++){
		    drawstring+="<td><button class='responsebutton' onclick='responseListener(\""+i+"\")' disabled=true>O</button></td>";
		}
	drawstring+="</tr>"+
	    "<tr><td colspan="+hm_options+">&nbsp</td></tr><tr>";
	for(var i =0;i<option_bookend_labels.length;i++){
	    drawstring+="<td>"+option_bookend_labels[i]+"</td>";
	}
	drawstring+="</tr></table>";
	document.getElementById(targdiv).innerHTML=drawstring;
    }
}

function spacerScreen(text){

    this.ppntID = localStorage.getItem("ppntID"); //copies same attributes as maketrial so you can be lazy and post all responses to the same db table.
    this.questiontext = "spacer_screen";
    this.text = text;
    this.hm_options=0;
    this.bookend_labels="Continue_button_only";

    this.drawMe = function(targdiv){
	var drawstring = "<p>"+this.text+"</p><button onclick='responseListener(\"continue\")'>Continue</button>";
	document.getElementById(targdiv).innerHTML = drawstring;
    }
}


function shuffle(a) { //via https://stackoverflow.com/questions/6274339/how-can-i-shuffle-an-array
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}
//****************************************************************************************************
//Stimuli

var stim = shuffle([
    "I asked what who saw?",
    "Who was for Mandy to poison awful?",
    "I am eating a mango and Gillian has too.",
    "Jason intended for to learn magic.",
    "Vampires have many become vegetarian.",
    "Zeus stinks to be omnipotent.",
    "I saw him ever.",
    "Did Mandy poison who?",
    "That monkey is ate the banana.",
    "Genie intoned the mirror.",
    "Mandy tried her to leave.",
    "Which king did you ask which city invaded?",
    "Enkidu do freed animals.",
    "Readings Shakespeare satisfied me.",
    "Has that we have arrived back at our starting point proved that the world is round?",
    "John is having fought Humbaba.",
    "Dragons have most been neutered.",
    "There arrived by Mandy.",
    "You said she liked yourself.",
    "Bill reading Shakespeare and Maureen singing Schubert satisfy me.",
    "Jason knew those Mandy had cast the spell.",
    "We all thought he to be unhappy.",
    "I arranged for to see her.",
    "John doesn’t ate the honey.",
    "Anson believed to be happy.",
    "Peter is those pigs.",
    "What she thought that was the poison was neutralised.",
    "Constant reading Shakespeare satisfied me.",
    "What Jason asked whether was the potion was ready.",
    "Peter is some happy pigs which can fly.",
    "Frantically at, Anson danced extremely Trade.",
    "We all thought him was unhappy.",
    "Ron asked that the potion was ready.",
    "I inquired could we leave early.",
    "Who was that Plato loved obvious?",
    "I said that that Jason was jealous annoyed Mandy.",
    "Mandy seems that has poisoned Jason.",
    "We believed to be omnipotent.",
    "Benjamin thought he would give the cloak to Lee and the cloak to Lee he gave.",
    "House the old collapsed.",
    "Mandy exclaimed if the potion was ready.",
    "Which city do you believe the claim that Philip would invade?",
    "By is eaten monkey banana that the being.",
    "Bill reading of Shakespeare satisfied me.",
    "John failed often Herbology.",
    "Benjamin said he would give the cloak to Lee and give the cloak he did to Lee.",
    "Enkidu quickly may free the animals.",
    "The monkey is ate the banana.",
    "It is true that I might be doing something other than going to the party.",
    "We need some technician to help us.",
    "The book that Anson lent to Jenny.",
    "When the executioner arrived, Hephaestus was asleep.",
    "That the answer is obvious upset Hermes.",
    "Moya’s football team loved her.",
    "Mandy poisoned more children than Jason did.",
    "What have you eaten?",
    "Jenny’s scraggy extremely demonic cat.",
    "Benjamin gave Lee the cloak.",
    "Bill reading Shakespeare satisfied me.",
    "We believed Zeus to be omnipotent.",
    "That bottle of water might have.",
    "Lucy’s disdain for Edmund.",
    "There is a description of Aristotle in the book.",
    "Julie and Jenny did.",
    "I have sent letters to Environmental Heath.",
    "Why did you kill Pegasus?",
    "Jason thought of defending the dragon.",
    "The consul’s gift of the gladiator to himself.",
    "Dracula thought that he was the Prince of Darkness.",
    "At Trade, Anson danced extremely frantically.",
    "People are in the garden.",
    "Where did Perseus see the Gorgon?",
    "Anson became the Mayor.",
    "The time when Alison and David soaked their feet was after dinner.",
    "Where has he put the cake?",
    "They depend on Mary.",
    "What she thought was that the poison was neutralised.",
    "Benjamin caused the book to go to Ross.",
    "I am to eat macaroni.",
    "It’s quarter past four.",
    "He looked up the number.",
    "In every club in London, people threw up their hands in the air.",
    "Julie and Jenny arrived first.",
    "Raffi slept well, and Gillian will too.",
    "Has the man eaten the cake?",
    "Ron definitely has bought a dog.",
    "Benjamin gave the cloak to Lee.",
    "I think she will go.",
    "The Gorgon is easy to slay.",
    "Humans love to eat them.",
    "I want to sing.",
    "Jason persuaded Mandy to desert her family.",
    "The Peter we all like was at the party.",
    "That bottle of water might have cracked open.",
    "Burn letters to Peter!",
    "John perhaps should be leaving.",
    "He queried what who threw?",
    "Where was for Mandy to go disastrous?",
    "I am pounding a nail and Gillian has too.",
    "Jason tried for to study Italian.",
    "Werewolves have many become tame.",
    "Robert stinks to be funny.",
    "I missed her ever.",
    "Did Mandy feed who?",
    "That dog is ate the pear.",
    "Genie recited the table.",
    "Mandy attempted her to flee.",
    "Which player did you ask which game played?",
    "Enkidu do released birds.",
    "Readings Tolkein confused me.",
    "Has that they have landed back at their launching point demonstrated that the wind is weak?",
    "Rosanne is having killed Peter.",
    "Cats have most been collared.",
    "There landed by Morgan.",
    "You claimed she hated yourself.",
    "Bill playing Bach and May reciting Shelly irritate me.",
    "Jason understood those Madeline had sparked the reaction.",
    "We all considered she to be excited.",
    "I organized for to visit him.",
    "William doesn’t jumped the fence",
    "Amber believed to be sad.",
    "Jackson is those dolphins.",
    "What she imagined that was the door was opened.",
    "Constant watching television nauseated me.",
    "What Jason inquired whether was the helicopter was landed.",
    "Neil is some wretched cats which can sing.",
    "Excitedly at, Andy waved very Zoo.",
    "They all believed her was sad.",
    "Ron asked that the cake was finished.",
    "I asked could we start sooner.",
    "Who was that Annie hated secret?",
    "I said that that Jason was unhappy upset Madea.",
    "Mike seems that has fed Toby.",
    "We thought to be qualified.",
    "Ben predicted he would send the envelope to Lee and the envelope to Lee he sent.",
    "Team the new won.",
    "Amelia exclaimed if the toast was ready.",
    "Which chest do you believe the claim that Henry would bury?",
    "By is destroyed armadillo watermelon that the being.",
    "Tom reciting of Gilgamesh disappointed me.",
    "Edward failed often Geography.",
    "Wallace said he would convey the message to Alice and convey the message he did to Alice.",
    "Ed slowly may release the fish.",
    "The rabbit is drank the water.",
    "It is true that I might be doing something other than fishing for salmon.",
    "We need some doctor to save us.",
    "The pen that Albert gave to Jane.",
    "When the teacher arrived, Henry was asleep.",
    "That the problem is easy annoyed Dave.",
    "Millie’s bowling team hated her.",
    "Mike fed more fish than Jones did.",
    "What have you drunk?",
    "Jenny’s glossy extremely forgetful bird.",
    "Benjamin passed Lee the hat.",
    "Greg singing Mozart depressed me.",
    "We considered Greg to be kind.",
    "That basket of rice might have.",
    "Lucy’s affection for David",
    "There is a picture of Albert in the leaflet.",
    "Julie and Jenny could.",
    "I have posted cards to Human Resources.",
    "Why did you save Archimedes?",
    "Jonas thought of attacking the dog.",
    "The overseer’s gift of the slave to himself.",
    "Oscar thought that he was the King of the Sea.",
    "At prom, Neil jumped extremely excitedly.",
    "Children are in the pool.",
    "Where did Peter find the officer?",
    "Andrew became the king.",
    "The time when Alison and David washed their hands was after dinner.",
    "Where has he left the book?",
    "They rely on Marty.",
    "What she imagined was that the door was opened.",
    "Linda caused the ball to go to Susan.",
    "I am to dance waltzes.",
    "It’s twenty past twelve.",
    "He sorted out the confusion.",
    "In every classroom in Georgia, students threw up their hands in the air.",
    "Julie and Jenny left last.",
    "Raffi ate well, and Gillian will too.",
    "Has the girl thrown the ball?",
    "Ron definitely has painted a wall.",
    "Benjamin passed the hat to Lee.",
    "I reckon she won’t stay.",
    "The Gorgon is hard to find.",
    "Fish love to find them.",
    "I want to swim.",
    "Jack convinced Michelle to desert her team.",
    "The Jasmine they all detest was at the opening.",
    "That basket of rice might have split open.",
    "Copy letters to Peter!",
    "John perhaps should be listening.",
    "I whistled what who drank?",
    "Where was for Mandy to teleport accidental?",
    "I am eating a skyscraper and Gillian has too.",
    "Jason intended for to burn water.",
    "Anvils have many become transparent.",
    "Ben stinks to be infinite.",
    "I related him ever.",
    "Did Alex recite who?",
    "That carrot is ate the stone.",
    "Genie intoned the footstool.",
    "Mandy tried her to evaporate.",
    "Which king did you ask which table exploded?",
    "Enkidu do freed stones.",
    "Readings Shakespeare blinded me.",
    "Has that we have arrived back at our starting point proved that the apple is metal?",
    "Gary is having colonized Alfred.",
    "Clouds have most been plastered.",
    "There hatched by Alice",
    "You said she digested yourself.",
    "Bill reading Picasso and Maureen humming Pythagoras satisfy me.",
    "Jason found those Madeline had destroyed the planets.",
    "We all thought he to be concrete.",
    "I planned for to accidentally miss.",
    "Albert doesn’t noticed the vuvuzela.",
    "Donny believed to be unconscious.",
    "Wendy is those swarms.",
    "What she sang that was the silence was stuffy.",
    "Constant training jellyfish deflated me.",
    "What Jason asked whether was the cake was saddled.",
    "Peter is some sleepy brushes which can translate.",
    "Persistently at, Anson fidgeted extremely Trade.",
    "We all thought him was purple.",
    "Ron asked that the window was stone.",
    "I inquired could we levitate heavily.",
    "Who was that Plato ate impossible?",
    "I said that that Jason was evaporated calmed Mandy.",
    "Mandy seems that has constructed Jason.",
    "We believed to be extinguished.",
    "Benjamin thought he would give the cloud to Lee and the cloud to Lee he gave.",
    "Rocket the old dug.",
    "Mandy exclaimed if the brick was stewed.",
    "Which atom do you believe the claim that Philip would invent?",
    "By is copied crocodile stick that the being.",
    "Bill snorting of Pythagoras delighted me.",
    "Enid passed often astrology.",
    "Benjamin said he would give the cloud to Lee and give the cloud he did to Lee.",
    "Enkidu slowly may teleport the animals.",
    "The yak is hugged the frog.",
    "It is true that I might be doing something other than swimming to the planet.",
    "We need some architect to massage us.",
    "The disease that Anson recommended to Jenny.",
    "When the book melted, Harry was upright.",
    "That the sonnet is invisible frustrated Clarke.",
    "Mandy’s hopping team transplanted her.",
    "Mike fed more paintings than Albert did.",
    "What have you emulsified?",
    "Jenny’s rabid extremely tall snake.",
    "Benjamin threw Lee the planet.",
    "Louis humming sign language oppressed me.",
    "We believed Gary to be non-existent.",
    "That basket of oxygen might have.",
    "Lucy’s pique for David",
    "There is a cartoon of Albert in the song.",
    "Julie and Jenny may.",
    "I have sold floss to Customs and Immigration",
    "Why did you drown the fish?",
    "Jonas thought of drinking the statue.",
    "The actress’s gift of the actor to himself",
    "Oscar thought he was the President of the wardrobe.",
    "At camp, Andrew sobbed extremely enthusiastically.",
    "Police are in the paradox.",
    "Where did Peter see the whisper?",
    "Andrew became the finch.",
    "The place where Alison and David soaked their feet was after dinner.",
    "Where has he hidden the mountain?",
    "They calculate on Marty.",
    "What she thought was that the idea was edible.",
    "Linda caused the shame to go to Fido.",
    "I am to swallow tractors.",
    "It’s a quarter past seventy.",
    "He looked up the watermelon.",
    "In every club in London, people threw up their tentacles in the air.",
    "Julie and Jenny fermented first.",
    "Raffi gasped well, and Gillian will too.",
    "Has the man inflated the cake?",
    "Ron definitely has barbecued an insight.",
    "Benjamin moved the emotion to Lee.",
    "I conclude she will ponder.",
    "The mattress is easy to slay.",
    "Fish need to inhale them.",
    "I want to irradiate.",
    "Jack persuaded Michelle to desert her sneakers.",
    "The Jasmine we all murdered was under the balloon.",
    "That bag of gophers might have shattered open.",
    "Prove letters to Peter!",
    "William perhaps should be drowning.",
    "The ancient manuscript that the grad student who the new card catalog had confused a great deal was studying in the library was missing a page.",
    "The ancient manuscript that the grad student who the new card catalog was studying in the library was missing a page.",
    "The ancient manuscript that the grad student who the new card catalog had confused a great deal was missing a page.",
    "The ancient manuscript that the grad student who the new card catalog had confused a great deal was studying in the library.",
    "The picture that the artist who the school had expelled for cheating was hurriedly copying was printed in a magazine.",
    "The picture that the artist who the school was hurriedly copying was printed in a magazine.",
    "The picture that the artist who the school had expelled for cheating was printed in a magazine.",
    "The picture that the artist who the school had expelled for cheating was hurriedly copying.",
    "The apartment that the maid who the service had sent over was cleaning every week was well decorated.",
    "The apartment that the maid who the service was cleaning every week was well decorated.",
    "The apartment that the maid who the service had sent over was well decorated.",
    "The apartment that the maid who the service had sent over was cleaning every week.",
    "The lecture that the professor who the newspaper story had just profiled in detail was teaching poorly was not well attended.",
    "The lecture that the professor who the newspaper story was teaching poorly was not well attended.",
    "The lecture that the professor who the newspaper story had just profiled in detail was not well attended.",
    "The lecture that the professor who the newspaper story had just profiled in detail was teaching poorly.",
    "The prayer that the monk who the religious fanatic had persecuted relentlessly was chanting every day was echoing in the empty church.",
    "The prayer that the monk who the religious fanatic was chanting every day was echoing in the empty church.",
    "The prayer that the monk who the religious fanatic had persecuted relentlessly was echoing in the empty church.",
    "The prayer that the monk who the religious fanatic had persecuted relentlessly was chanting every day.",
    "The carpenter who the craftsman that the peasant carried hurt supervised the apprentice.",
    "The carpenter who the pillar that the peasant carried hurt supervised the apprentice.",
    "The carpenter who the craftsman that the peasant carried supervised the apprentice.",
    "The carpenter who the pillar that the peasant carried supervised the apprentice.",
    "The worker who the tenant that the foreman looked for injured questioned the shepherd.",
    "The worker who the bucket that the foreman looked for injured questioned the shepherd.",
    "The worker who the tenant that the foreman looked for questioned the shepherd.",
    "The worker who the bucket that the foreman looked for questioned the shepherd.",
    "The painter who the musician that the father missed sheltered cooked for the artist.",
    "The painter who the hut that the father missed sheltered cooked for the artist.",
    "The painter who the musician that the father missed cooked for the artist.",
    "The painter who the hut that the father missed cooked for the artist.",
    "The pharmacist who the optician that the stranger saw troubled questioned the customer.",
    "The pharmacist who the button that the stranger saw troubled questioned the customer.",
    "The pharmacist who the optician that the stranger saw questioned the customer.",
    "The pharmacist who the button that the stranger saw questioned the customer.",
    "The dancer who the singer that the bystander admired hurt tipped the doorman.",
    "The dancer who the shoe that the bystander admired hurt tipped the doorman.",
    "The dancer who the singer that the bystander admired tipped the doorman.",
    "The dancer who the shoe that the bystander admired tipped the doorman.",
    "The clerk who the bureaucrat that the visitor forgotten about helped annoyed the neighbour.",
    "The clerk who the walking stick that the visitor forgotten about helped annoyed the neighbour.",
    "The clerk who the bureaucrat that the visitor forgotten about annoyed the neighbour.",
    "The clerk who the walking stick that the visitor forgotten about annoyed the neighbour.",
    "The conductor who the choirmaster that the worker ignored hit berated the musician.",
    "The conductor who the sponge that the worker ignored hit berated the musician.",
    "The conductor who the choirmaster that the worker ignored berated the musician.",
    "The conductor who the sponge that the worker ignored berated the musician.",
    "The cousin who the brother that the peasant described pleased hated the uncle.",
    "The cousin who the diamond that the peasant described pleased hated the uncle.",
    "The cousin who the brother that the peasant described hated the uncle.",
    "The cousin who the diamond that the peasant described hated the uncle.",
    "The slogan on the poster is offensive to vegetarians.",
    "The slogan on the posters are offensive to vegetarians.",
    "The slogans on the poster is offensive to vegetarians.",
    "The slogans on the posters are offensive to vegetarians.",
    "The name on the enormous highway billboard belongs to a local real-estate agent.",
    "The name on the enormous highway billboards belong to a local real-estate agent.",
    "The names on the enormous highway billboards belongs to a local real-estate agent.",
    "The names on the enormous highway billboards belong to a local real-estate agent.",
    "The problem in the wealthy suburban school has been brewing for years.",
    "The problem in the wealthy suburban schools have been brewing for years.",
    "The problems in the wealthy suburban school has been brewing for years.",
    "The problems in the wealthy suburban schools have been brewing for years.",
    "The mistake in the program is small but important.",
    "The mistake in the programs are small but important.",
    "The mistakes in the program is small but important.",
    "The mistakes in the programs are small but important.",
    "The memo from the accountant flutters to the floor unnoticed.",
    "The memo from the accountants flutter to the floor unnoticed.",
    "The memos from the accountant flutters to the floor unnoticed.",
    "The memos from the accountants flutter to the floor unnoticed.",
    "The warning from the expert falls on deaf ears.",
    "The warning from the experts fall on deaf ears.",
    "The warnings from the expert falls on deaf ears.",
    "The warnings from the experts fall on deaf ears.",
    "The key to the cabinet is on the table.",
    "The key to the cabinets are on the table.",
    "The keys to the cabinet is on the table.",
    "The keys to the cabinet are on the table.",
    "The bridge to the island is very crowded.",
    "The bridge to the islands are very crowded.",
    "The bridges to the island is very crowded.",
    "The bridges to the islands are very crowded.",
    "The tile used to cover the floor is from Morocco.",
    "The tile used to cover the floors are from Morocco.",
    "The tiles used to cover the floors is from Morocco.",
    "The tiles used to cover the floors are from Morocco.",
    "The actor hired to do the commercial is boring and uninspired.",
    "The actor hired to do the commercials are boring and uninspired.",
    "The actors hired to do the commercial is boring and uninspired.",
    "The actors hired to do the commercials are boring and uninspired.",
    "The mechanic who repaired the limousine’s rear tire is in a feud with my cousin’s friend’s family.",
    "The mechanic who repaired the limousine’s rear tires are in a feud with my cousin’s friend’s family.",
    "The mechanics who repaired the limousine’s rear tire is in a feud with my cousin’s friend’s family.",
    "The mechanics who repaired the limousine’s rear tires are in a feud with my cousin’s friend’s family.",
    "The professor who criticized the new dean accepts the new era grudgingly.",
    "The professor who criticized the new deans accept the new era grudgingly.",
    "The professors who criticized the new dean accepts the new era grudgingly.",
    "The professors who criticized the new deans accept the new era grudgingly.",
    "The boy that liked the colorful garter snake hides motionless behind a tree.",
    "The boy that liked the colorful garter snakes hide motionless behind a tree.",
    "The boys that liked the colorful garter snake hides motionless behind a tree.",
    "The boys that liked the colorful garter snakes hide motionless behind a tree.",
    "The table that the student painted looks nice.",
    "The table that the students painted look nice.",
    "The tables that the student painted looks nice.",
    "The tables that the students painted look nice.",
    "The soldier that the battalion’s senior officer accused was innocent.",
    "The soldier that the battalion’s senior officers accused were innocent.",
    "The soldiers that the battalion’s senior officer accused was innocent.",
    "The soldiers that the battalion’s senior officers accused were innocent.",
    "The dog that chased the truck is very muddy.",
    "The dog that chased the trucks are very muddy.",
    "The dogs that chased the truck is very muddy.",
    "The dogs that chased the trucks are very muddy."
]);


var hm_options = 6;
var conditions = shuffle([{question:"Is this sentence acceptable?",
			   optionlabels:["Highly </br>unacceptable",
					 "Unacceptable",
					 "Somewhat </br>unacceptable",
					 "Somewhat </br>acceptable",
					 "Acceptable",
					 "Highly </br>acceptable"],
			   myspacer:new spacerScreen("This block of questions asks you to judge how acceptable a sentence is. Here 'acceptable' means 'well-formed' or 'natural sounding'. The sentences here range in acceptability from very good to very poor, please use the rating scale to indicate where each sentence falls in this range.")},
			  {question:"Is this sentence grammatical?",
			   optionlabels:["Definitely </br>not</br>grammatical",
					 "Probably </br>not</br>grammatical",
					 "Possibly </br>not</br>grammatical",
					 "Possibly </br>grammatical",
					 "Probably </br>grammatical",
					 "Definitely </br>grammatical"],
			   myspacer:new spacerScreen("This block of questions asks you to judge if a sentence is grammatical or not. It doesn't matter if the sentence is ugly or even makes no sense: please answer 'Yes' if it follows the rules for sentence construction in English or 'No' if it does not.")},
			  {question:"Is this sentence meaningful?",
			   optionlabels:["Definitely </br>not</br>meaningful",
					 "Probably </br>not</br>meaningful",
					 "Possibly </br>not</br>meaningful",
					 "Possibly </br>meaningful",
					 "Probably </br>meaningful",
					 "Definitely </br>meaningful"],
			   myspacer: new spacerScreen("This block of questions asks you to judge if a sentence makes sense. Please answer 'Yes' if the sentence is meaningful, or 'No' if it is not.")
			  }])

var stim_counter = 0;
for( var whichcondition = 0; whichcondition < conditions.length; whichcondition++){
    trials.push(conditions[whichcondition].myspacer)
    for(i = 0 ; i < stim_per_condition; i++){
	trials.push(new makeTrial(conditions[whichcondition].question, stim[stim_counter], 6, conditions[whichcondition].optionlabels))
	stim_counter = stim_counter + 1;
    }
    //attention checks in gramm block. Because why not? Uncontrovertial answers apply there?
    if(conditions[whichcondition].question=="Is this sentence grammatical?"){
	trials.push(new makeTrial(conditions[whichcondition].question, "Him would have been fired.", 6, conditions[whichcondition].optionlabels));
	trials.push(new makeTrial(conditions[whichcondition].question, "Sarah expected to get a good grade.", 6, conditions[whichcondition].optionlabels));
    }
}

nextTrial();
