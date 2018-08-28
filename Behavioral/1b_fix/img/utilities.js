var instructions_text = function(nPracticeTrials){
	var instructions = ["Welcome this HIT!<br><br>Please read all the instructions very carefully. It takes some time, but otherwise you will not know what to do and will earn less money.<br><br>\
		We don't allow duplicate ID addresses. We apologize for any inconvenience this causes.<br><br>\
		This game has been tested on Firefox and Chrome. We can't guarantee that it works on other browsers.",
		"In this HIT, you will be hunting for space treasure on different planets.\
		<br><br><img style='margin:0px auto;display:block;height:200px' src='img/example_planets.png'/>", 
		"There are two spaceships that can get you to the planets. In the game, you can choose the left spaceship by pressing 'F', and the right spaceship by pressing 'J'.\
		<br><br><img style='margin:0px auto;display:block;height:100px' src='img/example_rockets.png'/><br>\
		The spaceship on the left usually goes to the green planet, and the spaceship on the right usually goes to the yellow planet.<br><br>\
		<img style='margin:0px auto;display:block;height:200px' src='img/example_planets.png'/><br>\
		But each spaceship has a 20% chance of getting lost and going to the purple planet by accident. \
		The chance of getting lost is the same for both spaceships. When it happens is completely random, and there's no way to plan for it.",
		"In other words, the game works like this:<br><br><img  style='margin:0px auto;display:block;height:400px' src='img/game_structure_noaliens.png'/><br>\
		These probabilities don't change throughout the game. Don't worry about memorizing them now; you'll get lots of practice.",
		"How do you actually get space treasure once you're on the planet? Well, the treasure is controlled by aliens who live on the planet.<br><br>\
		<img style='margin:0px auto;display:block;height:100px' src='img/example_aliens.png'/><br><br>\
		The green and yellow planets have two aliens, which you can select by pressing 'F' (left alien) or 'J' (right alien).",
		"<img style='margin:0px auto;display:block;height:100px' src='img/example_aliens_purple.png'/><br><br>\
		The purple planet also has two aliens, which you can select by pressing either 'F' (left) or 'J' (right). But on the purple planet, the two aliens are the same type. \
		<b>That means that they give identical amounts of treasure.</b><br><br>\
		For example, if the alien on the left is currently giving 3 pieces of treasure, then the alien on the right is also currently giving 3 pieces of treasure.\
		<b>So if you're on the purple planet, it doesn't matter which alien you pick.</b> They're exactly the same.",
		"The aliens will give you varying amounts of space treasure throughout the game . Sometimes, an alien will give you lots of treasure:\
		<br><br><img style='margin:0px auto;display:block' src='img/treasure_5.png'/><br>\
		Other times, it won't give you any treasure at all.<br><br>\
		<b>Treasure is good. The more treasure you get, the more bonus money you receive at the end of the experiment.</b> So you want to pay attention and pick the alien which is currently giving the most treasure.",
		"But sometimes, instead, of giving you treasure, an alien will give you antimatter: <br><br><img style='margin:0px auto;display:block' src='img/antimatter_3.png'/><br><br>\
		<b>Antimatter is bad. Every time you get antimatter, you lose some of your bonus money.</b> So you want to try to avoid aliens that are currently giving you antimatter.<br><br>\
		But remember, what an alien gives you changes throughout the experiment, so you have to stay on your toes. An alien that's giving you antimatter now might give you treasure later, or vice versa.",
		"So, to summarize the game: Aliens can give you treasure (which increases your bonus payment) or antimatter (which decreases your bonus payment). \
		There are different aliens on each planet, and what they give you is constantly changing. You'll have to figure out, through trial and error, which alien is the best at any given moment.<br><br>\
		<img style='margin:0px auto;display:block;height:100px' src='img/example_aliens_full.png'/><br>\
		To get to the alien you want, you'll choose a spaceship. \
		Usually, the spaceship on the left brings you to the green planet and the spaceship on the right brings you to the yellow planet.\
		But each spaceship has a 20% chance of getting lost and going to the purple planet. When this happens is completely random, and there's no way to plan for it.<br><br>\
		<img style='margin:0px auto;display:block;height:100px' src='img/example_rockets.png'/>",
		"In other words, the full game looks like this:<br><br><img  style='margin:0px auto;display:block;height:400px' src='img/game_structure.png'/><br>\
		Since you can only get to the purple planet by chance (and since the two purple aliens give identical amounts of treasure), you don't really have to think about the purple aliens as you play. You just have to focus on the green and yellow aliens.",
		"Let's give you some practice with the game. First you will do " + nPracticeTrials + " practice trials. These won't count towards your bonus payment.<br><br>\
		Our advice is to think carefully about your strategy, but also to trust your instincts. \
		By concentrating throughout the experiment you can increase the number of points you win by a lot. \
		There is an element of chance, but also room for skill.<br><br>Remember, press 'F' to select the spaceship or alien on the left, and press 'J' to select the spaceship or alien on the right.<br><br>\
		When you're ready to start, please continue."];
	return instructions
};

var instructions_2_text = function(pointsPerCent, nRealTrials){
	var instructions_2 = ["Great. Just to remind you, here is how the game works: <br><br><img  style='margin:0px auto;display:block;height:400px' src='img/game_structure.png'/>",
	"Now, you'll do " + nRealTrials + " real trials, which will count towards your bonus payment. \
	You will get a bonus payment of 1 cent for every " + pointsPerCent + " points you've earned by the end. On average people win about $0.75, but some have won more than $2.00. We'll start you off with 20 points.\
	<br><br>In these trials, we're only going to give you 2 seconds to make each choice. At first, you may feel rushed, but eventually it will feel like just the right amount of time.<br><br>\
	The game takes most people about 15-20 minutes. Good luck!"];
	return instructions_2
	
}

function createMemberInNormalDistribution(mean, std_dev){
	return mean + (gaussRandom()*std_dev);
}
/*
* Returns random number in normal distribution centering on 0.
* ~95% of numbers returned should fall between -2 and 2
*/
function gaussRandom() {
	var u = 2*Math.random()-1;
	var v = 2*Math.random()-1;
	var r = u*u + v*v;
	/*if outside interval [0,1] start over*/
	if(r == 0 || r > 1) return gaussRandom();

	var c = Math.sqrt(-2*Math.log(r)/r);
	return u*c;

	/* todo: optimize this algorithm by caching (v*c) 
	* and returning next time gaussRandom() is called.
	* left out for simplicity */
}

function shuffle(o){
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

var images = [
'img/p_earth_stim_1.png',
'img/p_earth_stim_2.png',
'img/p_earth_stim_1_deact.png',
'img/p_earth_stim_2_deact.png',
'img/green_planet.png',
'img/green_stim_1.png',
'img/green_stim_2.png',
'img/green_stim_1_deact.png',
'img/green_stim_2_deact.png',
'img/yellow_planet.png',
'img/yellow_stim_1.png',
'img/yellow_stim_2.png',
'img/yellow_stim_1_deact.png',
'img/yellow_stim_2_deact.png',
'img/purple_planet.png',
'img/purple_stim_1.png',
'img/purple_stim_2.png',
'img/purple_stim_1_deact.png',
'img/purple_stim_2_deact.png',
'img/antimatter_1.png',
'img/antimatter_2.png',
'img/antimatter_3.png',
'img/antimatter_4.png',
'img/antimatter_5.png',
'img/treasure_1.png',
'img/treasure_2.png',
'img/treasure_3.png',
'img/treasure_4.png',
'img/treasure_5.png',
'img/noreward.png',
'img/example_rockets.png',
'img/example_aliens.png',
'img/example_planets.png',
'img/game_structure_noaliens.png',
'img/game_structure.png'
];

function save_data(data, table_name){
	$.ajax({
		type:'post',
		cache: false,
		url: 'savedata.php', // change this to point to your php file.
		// opt_data is to add additional values to every row, like a subject ID
		// replace 'key' with the column name, and 'value' with the value.
		data: {
			table: table_name,
			json: JSON.stringify(data),
		},
		success: function(){
		}
	});
}

// JS equivalent of PHP's $_GET
function parseURLParams(url) {
    var queryStart = url.indexOf("?") + 1,
        queryEnd   = url.indexOf("#") + 1 || url.length + 1,
        query = url.slice(queryStart, queryEnd - 1),
        pairs = query.replace(/\+/g, " ").split("&"),
        parms = {}, i, n, v, nv;

    if (query === url || query === "") {
        return;
    }

    for (i = 0; i < pairs.length; i++) {
        nv = pairs[i].split("=");
        n = decodeURIComponent(nv[0]);
        v = decodeURIComponent(nv[1]);

        if (!parms.hasOwnProperty(n)) {
            parms[n] = [];
        }

        parms[n].push(nv.length === 2 ? v : null);
    }
    return parms;
}
