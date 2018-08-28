/**
* jspsych-space-points-stim
* Wouter Kool
*
* plugin for displaying a space and aliens version of the model-based/model-free 2-step task
*
**/

(function($) {
	jsPsych["space-points-stim"] = (function() {

		var plugin = {};
		
		var score = 0;
		
		var displayColor = '#0738db';
		var borderColor = '#197bff';
		var textColor = '#b8fff6';
		
		plugin.create = function(params) {

			params = jsPsych.pluginAPI.enforceArray(params, ['stimuli', 'choices']);
						
			var trials = new Array(params.nrtrials);
			
			for (var i = 0; i < trials.length; i++) {
				
				trials[i] = {};
				trials[i].choices = params.choices || [];
				trials[i].common_prob = params.common_prob || .8;
				trials[i].practice = params.practice || 0;
				trials[i].rews = params.rews;
				trials[i].init_points = params.init_points || 0;
				trials[i].subid = params.subid;
				trials[i].study_version = params.study_version;
				
				//trials[i].fixed_time = params.fixed_time || false;
				
				// timing parameters
				trials[i].feedback_time = params.feedback_time || 500;
				trials[i].ITI = params.ITI || 1000;
				trials[i].timeout_time = params.timeout_time || 2000;
				trials[i].timing_response = params.timing_response || 5000; // if -1, then wait for response forever
				trials[i].score_time = params.score_time || 1500;
				trials[i].totalscore_time = params.totalscore_time || 2000;
				
			}
			return trials;
			
		};
		
		plugin.trial = function(display_element, trial) {
			
			// if any trial variables are functions
			// this evaluates the function and replaces
			// it with the output of the function
			
			trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);
			
			progress = jsPsych.progress();
			if (progress.current_trial_local == 0) {
				score = trial.init_points;
			}
			
			var state = 1;
			var part = -1;
			
			var choice1 = -1;
			var choice2 = -1;
			
			var points = -1;
			var common = -1;
			var state2 = -1;
			
			var points = 0;
			
			//reverse_mapping_1 = Math.round(Math.random());
			//reverse_mapping_2 = Math.round(Math.random());
			reverse_mapping_1 = 0;
			reverse_mapping_2 = 0;

			if (reverse_mapping_1 == 0) {
				level_1_mapping = [1, 2];
			} else {
				level_1_mapping = [2, 1];
			}
			if (reverse_mapping_2 == 0) {
				level_2_mapping = [1, 2];
			} else {
				level_2_mapping = [2, 1];
			}
			//var level_1_mapping = shuffle([1,2]);
			//var level_2_mapping = shuffle([1,2]);
			
			var level_1_left = level_1_mapping[0];
			var level_1_right = level_1_mapping[1];
			var level_2_left = level_2_mapping[0];
			var level_2_right = level_2_mapping[1];

			var state_names = ["p_earth","green","yellow","purple"];
			var state_colors = [
				[5, 157, 190],
				[35, 63, 39],
				[240, 187, 57],
				[115, 34, 130]
			];
			
			/*if (trial.practice == 1) {
				var state_names = ["p_earth","green","yellow"];
				var state_colors = [
					[5, 157, 190],
					[35, 63, 39],
					[240, 187, 57]
				];
			} else {
				var state_names = ["earth","red","purple"];	
				var state_colors = [
					[5, 157, 190],
					[211, 0, 0],
					[115, 34, 130]
				];
			}*/	
			
			// store responses
			var setTimeoutHandlers = [];
			
			var keyboardListener = new Object;	
			
			var response = new Array(2);
			for (var i = 0; i < 2; i++) {	
				response[i] = {rt: -1, key: -1};
			}

			var timeouts = 0;

			// function to end trial when it is time
			var end_trial = function() {
				
				kill_listeners();
				kill_timers();
								
				// gather the data to store for the trial
				
				var trial_data = {
					"subject": trial.subid,
					"version": trial.study_version,
					"rt1": response[0].rt,
					"key1": response[0].key,
					"Action1": choice1,
					"rt2": response[1].rt,
					"key2": response[1].key,
					"Action2": choice2,
					"Re": points,
					"common": common,
					"S2": state2,
					"score": score,
					"practice": trial.practice,
					"reverse_mapping_1": reverse_mapping_1,
					"reverse_mapping_2": reverse_mapping_2,
					"vals": trial.rews.toString(),
					"timeouts": timeouts
				};
				
				jsPsych.data.write(trial_data);
				
				var handle_totalscore = setTimeout(function() {
					// clear the display
					display_element.html('');
					$('.jspsych-display-element').css('background-image', '');
				
					// move on to the next trial
					var handle_ITI = setTimeout(function() {
						jsPsych.finishTrial();
					}, trial.ITI);
					setTimeoutHandlers.push(handle_ITI);
				}, trial.totalscore_time);
				setTimeoutHandlers.push(handle_totalscore);
				
			};

			// function to handle responses by the subject
			var after_response = function(info) {
				
				kill_listeners();
				kill_timers();
								
				// only record the first response
				if (response[part].key == -1){
					response[part] = info;
				}

				/*if (trial.fixed_time == true && trial.timing_response > 0) {
					var extra_time = trial.timing_response-response[part].rt;
				} else {
					var extra_time = 0;
				}*/
				
				display_stimuli(2); //feedback
				
				if (part == 0) {
					left = level_1_left;
					right = level_1_right;
				} else {
					left = level_2_left;
					right = level_2_right;
				}
				
				switch(part) {
				case 0: // earth
					common = Math.random()<trial.common_prob;
					if (String.fromCharCode(response[part].key)==trial.choices[0]) { // left response
						choice1 = left;
					} else {
						choice1 = right;						
					}
					if (choice1 == 1) {
						if (common) {
							state2 = 2;
						} else {
							state2 = 4;
						}
					} else { // choice1 == 2
						if (common) {
							state2 = 3;
						} else {
							state2 = 4;
						}
					}
					state = state2;
					break;
				case 1:
					if (String.fromCharCode(response[part].key)==trial.choices[0]) { // left response
						choice2 = left;
					} else {
						choice2 = right;
					}
					points = trial.rews[state-2][choice2-1];
					score = score + points;
					break;
				}
								
				// show feedback
				var feedback_response = setTimeout(function() {
					
					if (part == 0){ // go to level 2
						display_element.html('');
						next_part();
					} else {
						display_stimuli(3);
						var handle_score = setTimeout(function() {
							display_stimuli(4);
							end_trial();
						}, trial.score_time);
						setTimeoutHandlers.push(handle_score);
					}
					
				}, trial.feedback_time);
				setTimeoutHandlers.push(feedback_response);
							
			};
			
			var display_stimuli = function(stage){
				
				kill_timers();
				kill_listeners();
				
				state_name = state_names[state-1];
				state_color = state_colors[state-1];
				
				if (part == 0) {
					left = level_1_left;
					right = level_1_right;
				} else {
					left = level_2_left;
					right = level_2_right;
				}
				
				if (stage==-1) { // timeout	
					/*$('#jspsych-space-daw-bottom-stim-left').html('<br><br>X');
					$('#jspsych-space-daw-bottom-stim-right').html('<br><br>X');
					$('#jspsych-space-daw-bottom-stim-left').css('background-image', 'url(img/'+state_name+'_stim_'+left+'_deact.png)');
					$('#jspsych-space-daw-bottom-stim-right').css('background-image', 'url(img/'+state_name+'_stim_'+right+'_deact.png)');*/
					display_element.html('<b>Please respond within the 2 second limit!</b>');
				}
				
				if (stage==1) { // choice stage
					display_element.html('');

					$('.jspsych-display-element').css('background-image', 'url("img/'+state_name+'_planet.png")');				
					
					/* Containers for possible rewards later */
					display_element.append($('<div>', {
						id: 'jspsych-space-daw-top-stim-left',
					}));

					display_element.append($('<div>', {
						id: 'jspsych-space-daw-top-stim-middle',
					}));

					display_element.append($('<div>', {
						id: 'jspsych-space-daw-top-stim-right',
					}));

					// Put faded rocket ship on top					
					/*if (state>1) {
						extra_text = "p_";

						display_element.append($('<div>', {
							style: 'background-image: url(img/'+extra_text+'earth_stim_'+choice1+'_deact.png)',
							id: 'jspsych-space-daw-top-stim-middle',
						}));
					}*/
					
					display_element.append($('<div>', {
						style: 'clear:both',
					}));
					
					/* rockets / aliens */

					// If we're not in the one weird state..
					if (state != 4) {
						display_element.append($('<div>', {
							style: 'background-image: url(img/'+state_name+'_stim_'+left+'.png)',
							id: 'jspsych-space-daw-bottom-stim-left',
						}));

						display_element.append($('<div>', {
							id: 'jspsych-space-daw-bottom-stim-middle',
						}));

						display_element.append($('<div>', {
							style: 'background-image: url(img/'+state_name+'_stim_'+right+'.png)',
							id: 'jspsych-space-daw-bottom-stim-right',
						}));

					} else { // If we are, pick one of the two (doesn't matter which) and put it in middle
						display_element.append($('<div>', {
							style: 'background-image: url(img/'+state_name+'_stim_'+right+'.png)',
							id: 'jspsych-space-daw-bottom-stim-middle',
						}));
					}
					
				}
				
				if (stage==2) { // feedback
					if (state != 4) {
						if (String.fromCharCode(response[part].key)==trial.choices[0]) { // left response
							$('#jspsych-space-daw-bottom-stim-right').css('background-image', 'url(img/'+state_name+'_stim_'+right+'_deact.png)');
							$('#jspsych-space-daw-bottom-stim-left').addClass('jspsych-space-daw-bottom-stim-border');
							$('#jspsych-space-daw-bottom-stim-left').css('border-color', 'rgba('+state_color[0]+','+state_color[1]+','+state_color[2]+', 1)');
						} else {
							$('#jspsych-space-daw-bottom-stim-left').css('background-image', 'url(img/'+state_name+'_stim_'+left+'_deact.png)');
							$('#jspsych-space-daw-bottom-stim-right').css('border-color', 'rgba('+state_color[0]+','+state_color[1]+','+state_color[2]+', 1)');
							$('#jspsych-space-daw-bottom-stim-right').addClass('jspsych-space-daw-bottom-stim-border');
						}
					} else {
						$('#jspsych-space-daw-bottom-stim-middle').css('border-color', 'rgba('+state_color[0]+','+state_color[1]+','+state_color[2]+', 1)');
						$('#jspsych-space-daw-bottom-stim-middle').addClass('jspsych-space-daw-bottom-stim-border');
					}
				}
				
				if (stage==3) { // reward
					if (state != 4) {
						if (String.fromCharCode(response[part].key)==trial.choices[0]) { // left response
							if (points>0) {
								$('#jspsych-space-daw-top-stim-left').css('background-image', 'url(img/treasure_'+points+'.png)');
							}
							if (points<0) {
								$('#jspsych-space-daw-top-stim-left').css('background-image', 'url(img/antimatter_'+(-1*points)+'.png)');
							}
							if (points==0) {
								$('#jspsych-space-daw-top-stim-left').css('background-image', 'url(img/noreward.png)');
							}
						} else {
							if (points>0) {
								$('#jspsych-space-daw-top-stim-right').css('background-image', 'url(img/treasure_'+points+'.png)');
							}
							if (points<0) {
								$('#jspsych-space-daw-top-stim-right').css('background-image', 'url(img/antimatter_'+(-1*points)+'.png)');
							}
							if (points==0) {
								$('#jspsych-space-daw-top-stim-right').css('background-image', 'url(img/noreward.png)');
							}
						}
					} else {
						if (points>0) {
							$('#jspsych-space-daw-top-stim-middle').css('background-image', 'url(img/treasure_'+points+'.png)');
						}
						if (points<0) {
							$('#jspsych-space-daw-top-stim-middle').css('background-image', 'url(img/antimatter_'+(-1*points)+'.png)');
						}
						if (points==0) {
							$('#jspsych-space-daw-top-stim-middle').css('background-image', 'url(img/noreward.png)');
						}
					}
				}
				
				if (stage==4) { // 
					display_element.html('');
										
					if (points > 0) {
						picture = 'img/treasure_'+points+'.png';
					}
					if (points < 0) {
						picture = 'img/antimatter_'+Math.abs(points)+'.png';
					}
					if (points == 0) {
						picture = 'img/noreward.png';
					}
					
					$('.jspsych-display-element').css('background-image', 'url("img/earth_planet.png")');
					display_element.append($('<div>', {
						id: 'jspsych-space-daw-top-rewards',
					}));
					$('#jspsych-space-daw-top-rewards').append($('<div id="jspsych-space-daw-top-rewards-container"><img src="'+picture+'"></div>'))
					
					$('#jspsych-space-daw-top-rewards').append($('<div id="jspsych-space-daw-top-rewards-text"> = '+points+'</div>'))
					
					$('#jspsych-space-daw-top-rewards').append($('<div style="clear:both; height:20px"></div>'))
					$('#jspsych-space-daw-top-rewards').append($('<div id="jspsych-space-daw-top-rewards-text">total score: '+score+'</div>'))

					display_element.append($('<div>', {
						id: 'jspsych-space-daw-bottom-alien',
					}));

					if (state != 4) {
						if (String.fromCharCode(response[part].key)==trial.choices[0]) { 
							$('#jspsych-space-daw-bottom-alien').append($('<img src="img/'+state_name+'_stim_'+left+'.png">'));
						} else {
							$('#jspsych-space-daw-bottom-alien').append($('<img src="img/'+state_name+'_stim_'+right+'.png">'));
						}
					} else {
						$('#jspsych-space-daw-bottom-alien').append($('<img src="img/'+state_name+'_stim_'+right+'.png">'));
					}
				}
				
			}
			
			var start_response_listener = function(){
				if(JSON.stringify(trial.choices) != JSON.stringify(["none"])) {
					var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
						callback_function: after_response,
						valid_responses: trial.choices,
						rt_method: 'date',
						persist: false,
						allow_held_key: false
					});
				}
			}
			
			var kill_timers = function(){
				for (var i = 0; i < setTimeoutHandlers.length; i++) {
					clearTimeout(setTimeoutHandlers[i]);
				}
			}
			
			var kill_listeners = function(){
				// kill keyboard listeners
				if(typeof keyboardListener !== 'undefined'){
					jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
				}
			}
			
			var next_part = function(){
				
				part = part + 1;
				
				kill_timers();
				kill_listeners();
				
				display_stimuli(1);
				start_response_listener();
								
				if (trial.timing_response>0) {	
					var handle_response = setTimeout(function() {
						kill_listeners();
						display_stimuli(-1);
						var handle_timeout = setTimeout(function() {
							//end_trial();
							part = -1;
							state = 1;
							timeouts++;
							next_part();
						}, trial.timeout_time);
						setTimeoutHandlers.push(handle_timeout);
					}, trial.timing_response);
					setTimeoutHandlers.push(handle_response);
				}
			}			
			
			next_part();
			
		};
		
		return plugin;
	})();
})(jQuery);
