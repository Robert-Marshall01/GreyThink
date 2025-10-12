/*
VERY IMPORTANT: GitHub Copilot AI-Generated Code, please review and test thoroughly.
IMPORTANT: This program is made by an indie developer, not a professional.
IMPORTANT: This program is not endorsed nor sponsored by major tech firms like Google and Microsoft.

NOTE: To test, open the calcal html pages in a web browser,
or, install the Web Dev extension and run on vscode.

Food Searchbar now uses the USDA FoodData Central API. You must provide your own API key.
Instructions: Get your API key at https://fdc.nal.usda.gov/api-key-signup.html
Citation: USDA FoodData Central API documentation: https://fdc.nal.usda.gov/api-guide.html
Replace 'YOUR_USDA_API_KEY' below with your actual API key.
Database starts at line 555.
*/
// Sign in page logic
$(document).ready(function() {
	// Global strong password validation for all forms with password input
	var strongRegex = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]).{8,}$/;
	$('form').each(function() {
		var $form = $(this);
		var $password = $form.find('input[type="password"]');
		var $submit = $form.find('button[type="submit"], input[type="submit"]');
		if ($password.length && $submit.length) {
			function checkPasswordStrength() {
				var password = $password.val();
				if (!strongRegex.test(password)) {
					$form.find('.password-strength-message').remove();
					$submit.prop('disabled', true);
					var $msg = $('<div class="password-strength-message mt-2"></div>').text('Password must be at least 8 characters and include uppercase, lowercase, number, and special character.').css('color', 'red');
					$form.append($msg);
					return false;
				} else {
					$form.find('.password-strength-message').remove();
					$submit.prop('disabled', false);
					return true;
				}
			}
			$password.on('input', checkPasswordStrength);
			$form.on('submit', function(e) {
				if (!checkPasswordStrength()) {
					e.preventDefault();
					return false;
				}
			});
			// Initial check in case browser autofills
			checkPasswordStrength();
		}
	});
	// Universal input sanitization for all forms on every page
	$('form').each(function() {
		var $form = $(this);
		$form.on('submit', function(e) {
			var safe = true;
			$form.find('input').each(function() {
				var val = $(this).val();
				var sanitized = val.replace(/["'`;<>]/g, '');
				sanitized = sanitized.substring(0, 64);
				$(this).val(sanitized);
				if (val !== sanitized) {
					safe = false;
				}
			});
			if (!safe) {
				// Try to find a message div in the form, or create one
				var $msg = $form.find('.input-sanitize-message');
				if ($msg.length === 0) {
					$msg = $('<div class="input-sanitize-message mt-2"></div>');
					$form.append($msg);
				}
				$msg.text('Some characters were removed for security.').css('color', 'orange');
			}
		});
	});
	// On page load, initialize nutrition goal fields with values from backend
	if ($('#goal-weight-goal').length) {
    var username = localStorage.getItem('calcal_last_user');
    if (username) {
        $.ajax({
            url: 'http://127.0.0.1:5000/get_goals',
            method: 'POST',
            contentType: 'application/json',
            data: JSON.stringify({ username: username }),
            success: function(res) {
                if (res.success && res.goals) {
                    var goals = res.goals;
                    if (goals.weightGoal && $('#goal-weight-goal').val() !== goals.weightGoal) {
                        $('#goal-weight-goal').val(goals.weightGoal);
                    }
                    var fields = ['calories','protein','fats','cholesterol','carbs','sugar','fiber','sodium'];
                    fields.forEach(function(field) {
                        var valKey = field + 'Value';
                        var fieldId = '#goal-' + field + '-value';
                        if ($(fieldId).length && goals[valKey] !== undefined) {
                            $(fieldId).val(goals[valKey]);
                        }
                    });
                }
            }
        });
    }
	}
	// Universal Nutrition Goal Logic
	function setNutritionDefaults(goal) {
		var username = localStorage.getItem('calcal_last_user');
		var defaults = {
			maintain: { caloriesValue: 2000, proteinValue: 50, fatsValue: 70, cholesterolValue: 300, carbsValue: 275, sugarValue: 50, fiberValue: 28, sodiumValue: 2300 },
			lose:     { caloriesValue: 1500, proteinValue: 60, fatsValue: 50, cholesterolValue: 200, carbsValue: 200, sugarValue: 40, fiberValue: 32, sodiumValue: 2000 },
			gain:     { caloriesValue: 2500, proteinValue: 80, fatsValue: 90, cholesterolValue: 350, carbsValue: 350, sugarValue: 60, fiberValue: 30, sodiumValue: 2500 }
		};
		var selected = defaults[goal] || defaults['maintain'];
		// Update UI fields
		Object.keys(selected).forEach(function(key) {
			var field = key.replace('Value','');
			var valueId = '#goal-' + field + '-value';
			if ($(valueId).length) {
				$(valueId).val(selected[key]);
			}
			var typeId = '#goal-' + field + '-type';
			if ($(typeId).length) {
				if (field === 'protein' || field === 'fiber') {
					$(typeId).val('over');
				} else {
					if (goal === 'gain') {
						$(typeId).val('over');
					} else {
						$(typeId).val('under');
					}
				}
			}
		});
		// Save to backend
		$.ajax({
			url: 'http://127.0.0.1:5000/update_goals',
			method: 'POST',
			contentType: 'application/json',
			data: JSON.stringify({ username: username, goals: Object.assign({ weightGoal: goal }, selected) }),
			success: function(res) {
				// Optionally re-render food list and donuts if needed
				if (typeof renderFoodListAndDonuts === 'function') {
					fetchFoodListAndRender();
				}
			}
		});
	}

	// Event handler for weight goal change
	if ($('#goal-weight-goal').length) {
		$('#goal-weight-goal').on('change', function() {
			var goal = $(this).val();
			setNutritionDefaults(goal);
		});
	}

	// Apply font size from backend settings
	function applyFontSize(fontSize) {
		$('body').removeClass('font-normal font-large font-xlarge');
		if (fontSize === 'large') {
			$('body').addClass('font-large');
		} else if (fontSize === 'xlarge') {
			$('body').addClass('font-xlarge');
		} else {
			$('body').addClass('font-normal');
		}
	}

	// Font size change handler (settings page or accessibility controls)
	$(document).on('change', '#font-size-select', function() {
		var fontSize = $(this).val();
		applyFontSize(fontSize);
		var username = localStorage.getItem('calcal_last_user');
		if (username) {
			$.ajax({
				url: 'http://127.0.0.1:5000/update_settings',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, fontSize: fontSize }),
			});
		}
	});

	// On page load, apply font size from backend settings
	var username = localStorage.getItem('calcal_last_user');
	if (username) {
		$.ajax({
			url: 'http://127.0.0.1:5000/get_settings',
			method: 'POST',
			contentType: 'application/json',
			data: JSON.stringify({ username: username }),
			success: function(res) {
				if (res.success && res.settings) {
					var fontSize = res.settings.fontSize || 'normal';
					applyFontSize(fontSize);
				} else {
					applyFontSize('normal');
				}
			},
			error: function() {
				applyFontSize('normal');
			}
		});
	} else {
		applyFontSize('normal');
	}
	// Caloric Expenditure Section: Calculate BMR and compare to intake
	if ($('#caloric-expenditure-section').length) {
		var username = localStorage.getItem('calcal_last_user');
		if (!username) {
			$('#caloric-expenditure-details').html('<span class="text-warning">Please sign in to view expenditure.</span>');
			return;
		}
		// Fetch user info, goals, and food list from backend
		$.when(
			$.ajax({
				url: 'http://127.0.0.1:5000/get_settings',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username })
			}),
			$.ajax({
				url: 'http://127.0.0.1:5000/get_goals',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username })
			}),
			$.ajax({
				url: 'http://127.0.0.1:5000/get_foods',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username })
			})
		).done(function(settingsRes, goalsRes, foodsRes) {
			var user = settingsRes[0].settings || {};
			var goals = goalsRes[0].goals || {};
			var foodList = foodsRes[0].foods || [];
			// Get user info
			var weight = Number(user.weight) || 0;
			var height = Number(user.height) || 0;
			var age = Number(user.age) || 0;
			var gender = (user.gender || '').toLowerCase();
			var activity = (user.activity || '').toLowerCase();
			var units = user.units || 'metric';
			// Convert to metric if needed
			if (units === 'imperial') {
				weight = weight / 2.20462; // lb to kg
				height = height * 2.54; // in to cm
			}
			// Calculate BMR (Mifflin-St Jeor)
			var bmr = 0;
			if (weight && height && age) {
				if (gender === 'male') {
					bmr = 10 * weight + 6.25 * height - 5 * age + 5;
				} else if (gender === 'female') {
					bmr = 10 * weight + 6.25 * height - 5 * age - 161;
				}
			}
			// Activity multiplier
			var activityFactor = 1.2; // sedentary default
			if (activity.includes('light')) activityFactor = 1.375;
			else if (activity.includes('moderate')) activityFactor = 1.55;
			else if (activity.includes('active')) activityFactor = 1.725;
			else if (activity.includes('very')) activityFactor = 1.9;
			var tdee = Math.round(bmr * activityFactor);
			// Get today's caloric intake
			var totalCalories = 0;
			foodList.forEach(function(item) {
				totalCalories += Number(item.calories) || 0;
			});
			totalCalories = Math.round(totalCalories);
			// Render results
			var html = '';
			if (bmr && tdee) {
				html += '<div class="mb-2">';
				var maxBar = 4000;
				html += '<strong>BMR:</strong> ' + Math.round(bmr) + ' kcal/day';
				var bmrPercent = Math.min(100, Math.round((bmr / maxBar) * 100));
				html += '<div class="progress mb-2" style="height: 20px;">';
				html += '<div class="progress-bar bg-info" role="progressbar" style="width: ' + bmrPercent + '%;" aria-valuenow="' + bmrPercent + '" aria-valuemin="0" aria-valuemax="100">' + Math.round(bmr) + ' kcal' + '</div>';
				html += '</div>';
				html += '<strong>Estimated Daily Expenditure (TDEE):</strong> ' + tdee + ' kcal/day';
				var tdeePercent = Math.min(100, Math.round((tdee / maxBar) * 100));
				html += '<div class="progress mb-2" style="height: 20px;">';
				html += '<div class="progress-bar bg-primary" role="progressbar" style="width: ' + tdeePercent + '%;" aria-valuenow="' + tdeePercent + '" aria-valuemin="0" aria-valuemax="100">' + tdee + ' kcal' + '</div>';
				html += '</div>';
				html += '<strong>Caloric Intake Today:</strong> ' + totalCalories + ' kcal';
				var intakePercent = Math.min(100, Math.round((totalCalories / maxBar) * 100));
				html += '<div class="progress mb-2" style="height: 20px;">';
				html += '<div class="progress-bar bg-success" role="progressbar" style="width: ' + intakePercent + '%;" aria-valuenow="' + intakePercent + '" aria-valuemin="0" aria-valuemax="100">' + totalCalories + ' kcal' + '</div>';
				html += '</div>';
				var diff = totalCalories - tdee;
				var diffLabel = '';
				var diffColor = '';
				var weightGoal = goals.weightGoal || 'maintain';
				if (diff > 0) {
					diffLabel = diff + ' kcal over';
					if (weightGoal === 'lose') diffColor = 'bg-danger';
					else if (weightGoal === 'maintain') diffColor = 'bg-warning';
					else if (weightGoal === 'gain') diffColor = 'bg-success';
				} else if (diff < 0) {
					diffLabel = Math.abs(diff) + ' kcal under';
					if (weightGoal === 'lose') diffColor = 'bg-success';
					else if (weightGoal === 'maintain') diffColor = 'bg-warning';
					else if (weightGoal === 'gain') diffColor = 'bg-danger';
				} else {
					diffLabel = 'Matched';
					if (weightGoal === 'lose') diffColor = 'bg-warning';
					else if (weightGoal === 'maintain') diffColor = 'bg-success';
					else if (weightGoal === 'gain') diffColor = 'bg-warning';
				}
				var baseProgress = 2000;
				var progressValue = 0;
				if (diff < 0) {
					progressValue = baseProgress - Math.abs(diff);
				} else if (diff > 0) {
					progressValue = baseProgress + diff;
				} else {
					progressValue = baseProgress;
				}
				var diffPercent = Math.min(100, Math.round((progressValue / 4000) * 100));
				html += '<strong>Calories Above/Below Expenditure:</strong> ' + diffLabel;
				html += '<div class="progress mb-2" style="height: 20px;">';
				html += '<div class="progress-bar ' + diffColor + '" role="progressbar" style="width: ' + diffPercent + '%;" aria-valuenow="' + diffPercent + '" aria-valuemin="0" aria-valuemax="100">' + diffLabel + '</div>';
				html += '</div>';
				html += '</div>';
			} else {
				html += '<span class="text-warning">Please complete your user profile to see BMR and expenditure.</span>';
			}
			$('#caloric-expenditure-details').html(html);
		}).fail(function() {
			$('#caloric-expenditure-details').html('<span class="text-danger">Failed to load expenditure data.</span>');
		});
	}
	// Helper to round all values in an object to integers
	function roundTotals(obj) {
		var rounded = {};
		Object.keys(obj).forEach(function(key) {
			rounded[key] = Math.round(Number(obj[key]) || 0);
		});
		return rounded;
	}
	// Custom food appendment logic
	if ($('#custom-food-form').length) {
		$('#custom-food-form').on('submit', function(e) {
			e.preventDefault();
			var username = localStorage.getItem('calcal_last_user');
			var foodListKey = 'calcal_foods_' + username;
			var foodList = JSON.parse(localStorage.getItem(foodListKey) || '[]');
			if (!username) {
				$('#custom-food-message').css('color', 'red').text('Please sign in first.');
				return;
			}
			// Get custom food values from form
			var foodItem = {
				foodName: $('#custom-food-name').val().trim(),
				id: 'custom-' + Date.now(),
				calories: $('#custom-food-calories').val().trim(),
				protein: $('#custom-food-protein').val().trim(),
				fats: $('#custom-food-fats').val().trim(),
				cholesterol: $('#custom-food-cholesterol').val().trim(),
				carbs: $('#custom-food-carbs').val().trim(),
				sugar: $('#custom-food-sugar').val().trim(),
				fiber: $('#custom-food-fiber').val().trim(),
				sodium: $('#custom-food-sodium').val().trim()
			};
			$.ajax({
				url: 'http://127.0.0.1:5000/add_food',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, food: foodItem }),
				success: function(res) {
					if (res.success) {
						$('#custom-food-form')[0].reset();
						$('#custom-food-message').css('color', 'green').text('Custom food added!');
						setTimeout(function() {
							location.reload(true);
						}, 800);
					} else {
						$('#custom-food-message').css('color', 'red').text(res.error || 'Failed to add food.');
					}
				},
				error: function(xhr) {
					$('#custom-food-message').css('color', 'red').text('Failed to add food.');
				}
			});
		});
	}
	// Modular function to render food list and donut bars
	function renderFoodListAndDonuts(foodList) {
		$('#food-list').empty();
		$('#donut-bars').empty();
		var username = localStorage.getItem('calcal_last_user');
		if (!username) return;
		// Render food list items
		$('#food-list').empty();
		var maxCalories = 800;
		var html = '';
		for (var i = 0; i < (foodList || []).length; i++) {
			var item = foodList[i];
			if (i % 2 === 0) html += '<div class="row mb-2">';
			var calories = Number(item.calories) || 0;
		var percent = Math.min(100, Math.round((calories / maxCalories) * 100));
			html += '<div class="col-md-6 mb-2">'
				+ '<div class="card h-100 shadow-sm">'
				+ '<div class="card-body">'
				+ '<h5 class="card-title">' + (item.foodName || 'Unnamed Food') + '</h5>'
				+ '<p class="card-text mb-2">Calories: ' + calories + ' kcal</p>'
				+ '<div class="progress mb-2" style="height: 18px;">'
				+ '<div class="progress-bar bg-info" role="progressbar" style="width: ' + percent + '%;" aria-valuenow="' + percent + '" aria-valuemin="0" aria-valuemax="100"></div>'
				+ '</div>'
				+ '<p class="card-text small">Protein: ' + (item.protein || 0) + 'g, Fats: ' + (item.fats || 0) + 'g, Carbs: ' + (item.carbs || 0) + 'g, Sugar: ' + (item.sugar || 0) + 'g, Fiber: ' + (item.fiber || 0) + 'g, Sodium: ' + (item.sodium || 0) + 'mg, Cholesterol: ' + (item.cholesterol || 0) + 'mg</p>'
				+ '</div></div></div>';
			if (i % 2 === 1 || i === (foodList.length - 1)) html += '</div>';
		}
		$('#food-list').html(html);
		// Fetch nutritional goals from backend
		$.ajax({
			url: 'http://127.0.0.1:5000/get_goals',
			method: 'POST',
			contentType: 'application/json',
			data: JSON.stringify({ username: username }),
			success: function(res) {
				var goals = res.goals || {};
				// Use backend values for donut bars
				var maxVals = {
					calories: Number(goals.caloriesValue) || 2000,
					protein: Number(goals.proteinValue) || 50,
					fats: Number(goals.fatsValue) || 78,
					cholesterol: Number(goals.cholesterolValue) || 300,
					carbs: Number(goals.carbsValue) || 275,
					sugar: Number(goals.sugarValue) || 50,
					fiber: Number(goals.fiberValue) || 28,
					sodium: Number(goals.sodiumValue) || 2300
				};
				var labels = {
					calories: 'Calories', protein: 'Protein', fats: 'Fats', cholesterol: 'Cholesterol', carbs: 'Carbs', sugar: 'Sugar', fiber: 'Fiber', sodium: 'Sodium'
				};
				// Calculate totals from foodList
				var totals = {
					calories: 0, protein: 0, fats: 0, cholesterol: 0, carbs: 0, sugar: 0, fiber: 0, sodium: 0
				};
				(foodList || []).forEach(function(item) {
					totals.calories += Number(item.calories) || 0;
					totals.protein += Number(item.protein) || 0;
					totals.fats += Number(item.fats) || 0;
					totals.cholesterol += Number(item.cholesterol) || 0;
					totals.carbs += Number(item.carbs) || 0;
					totals.sugar += Number(item.sugar) || 0;
					totals.fiber += Number(item.fiber) || 0;
					totals.sodium += Number(item.sodium) || 0;
				});
				// Render donut bars with color based on Under/Over setting
				var donutHtml = '';
				Object.keys(totals).forEach(function(key) {
					var value = Math.round(totals[key]);
					var max = maxVals[key];
					var percent = max ? Math.min(100, Math.round((value / max) * 100)) : 0;
					// Get Under/Over setting from goals
					var typeKey = key + 'Type';
					var type = (goals[typeKey] || '').toLowerCase();
					var color = '#0dcaf0';
					if (type === 'under') color = '#198754'; // green
					else if (type === 'over') color = '#fd7e14'; // orange
					var canvasId = 'donut-' + key;
					donutHtml += '<div class="text-center">'
						+ '<canvas id="' + canvasId + '" width="120" height="120"></canvas>'
						+ '<div class="fw-semibold mt-2">' + labels[key] + '</div>'
						+ '<div class="small">' + value + ' / ' + max + '</div>'
						+ '</div>';
				});
				$('#donut-bars').html(donutHtml);
				Object.keys(totals).forEach(function(key) {
					var value = Math.round(totals[key]);
					var max = maxVals[key];
					var percent = max ? Math.min(100, Math.round((value / max) * 100)) : 0;
					var typeKey = key + 'Type';
					var type = (goals[typeKey] || '').toLowerCase();
					var color = '#0dcaf0';
					if (type === 'under') color = '#198754'; // green
					else if (type === 'over') color = '#fd7e14'; // orange
					var canvas = document.getElementById('donut-' + key);
					if (canvas && canvas.getContext) {
						var ctx = canvas.getContext('2d');
						ctx.clearRect(0, 0, 120, 120);
						ctx.beginPath();
						ctx.arc(60, 60, 50, 0, 2 * Math.PI);
						ctx.strokeStyle = '#e9ecef';
						ctx.lineWidth = 16;
						ctx.stroke();
						ctx.beginPath();
						ctx.arc(60, 60, 50, -0.5 * Math.PI, (percent / 100) * 2 * Math.PI - 0.5 * Math.PI);
						ctx.strokeStyle = color;
						ctx.lineWidth = 16;
						ctx.stroke();
					}
				});
			},
			error: function() {
				// Fallback to defaults if backend fails
				var maxVals = {
					calories: 2000, protein: 50, fats: 78, cholesterol: 300, carbs: 275, sugar: 50, fiber: 28, sodium: 2300
				};
				var labels = {
					calories: 'Calories', protein: 'Protein', fats: 'Fats', cholesterol: 'Cholesterol', carbs: 'Carbs', sugar: 'Sugar', fiber: 'Fiber', sodium: 'Sodium'
				};
				var totals = {
					calories: 0, protein: 0, fats: 0, cholesterol: 0, carbs: 0, sugar: 0, fiber: 0, sodium: 0
				};
				(foodList || []).forEach(function(item) {
					totals.calories += Number(item.calories) || 0;
					totals.protein += Number(item.protein) || 0;
					totals.fats += Number(item.fats) || 0;
					totals.cholesterol += Number(item.cholesterol) || 0;
					totals.carbs += Number(item.carbs) || 0;
					totals.sugar += Number(item.sugar) || 0;
					totals.fiber += Number(item.fiber) || 0;
					totals.sodium += Number(item.sodium) || 0;
				});
				var donutHtml = '';
				Object.keys(totals).forEach(function(key) {
					var value = Math.round(totals[key]);
					var max = maxVals[key];
					var canvasId = 'donut-' + key;
					donutHtml += '<div class="text-center">'
						+ '<canvas id="' + canvasId + '" width="120" height="120"></canvas>'
						+ '<div class="fw-semibold mt-2">' + labels[key] + '</div>'
						+ '<div class="small">' + value + ' / ' + max + '</div>'
						+ '</div>';
				});
				$('#donut-bars').html(donutHtml);
				Object.keys(totals).forEach(function(key) {
					var value = Math.round(totals[key]);
					var max = maxVals[key];
					var percent = max ? Math.min(100, Math.round((value / max) * 100)) : 0;
					var color = '#0dcaf0';
					var canvas = document.getElementById('donut-' + key);
					if (canvas && canvas.getContext) {
						var ctx = canvas.getContext('2d');
						ctx.clearRect(0, 0, 120, 120);
						ctx.beginPath();
						ctx.arc(60, 60, 50, 0, 2 * Math.PI);
						ctx.strokeStyle = '#e9ecef';
						ctx.lineWidth = 16;
						ctx.stroke();
						ctx.beginPath();
						ctx.arc(60, 60, 50, -0.5 * Math.PI, (percent / 100) * 2 * Math.PI - 0.5 * Math.PI);
						ctx.strokeStyle = color;
						ctx.lineWidth = 16;
						ctx.stroke();
					}
				});
			}
		});
	}
	// Food search logic using USDA FoodData Central API
	if ($('#food-search-form').length) {
		$('#food-search-form').on('submit', function(e) {
			e.preventDefault();
			var searchTerm = $('#food-search-input').val().trim();
			if (!searchTerm) {
				$('#food-search-results').html('<span class="text-danger">Please enter a food name.</span>');
				return;
			}
			$('#food-search-results').html('<span class="text-info">Searching USDA database...</span>');
			var apiKey = 'INSERT_YOUR_API_KEY_HERE'; // <-- Replace with your actual API key
			// Step 1: Search for food items
			$.ajax({
				url: 'https://api.nal.usda.gov/fdc/v1/foods/search',
				method: 'GET',
				data: {
					api_key: apiKey,
					query: searchTerm,
					pageSize: 1
				},
				success: function(data) {
					if (data.foods && data.foods.length > 0) {
						var food = data.foods[0];
						var foodName = food.description || searchTerm;
						var fdcId = food.fdcId;
						// Step 2: Get detailed nutrients for the selected food
						$.ajax({
							url: 'https://api.nal.usda.gov/fdc/v1/food/' + fdcId,
							method: 'GET',
							data: { api_key: apiKey },
							success: function(detail) {
								var nutrients = {};
								if (detail.foodNutrients) {
									// Debug: print all nutrient names and values
									console.log('USDA Nutrients for FDC ID', detail.fdcId);
									detail.foodNutrients.forEach(function(n) {
										if (!n || !n.nutrientName) return;
										var name = String(n.nutrientName).toLowerCase();
										var value = n.value;
										console.log('Nutrient:', name, 'Value:', value);
										// Calories
										if ((name.includes('energy') && name.includes('kcal')) || name === 'energy' || name === 'energy, kcal') nutrients.calories = value;
										// Protein
										if (name === 'protein' || name.includes('protein')) nutrients.protein = value;
										// Fats
										if (name === 'total lipid (fat)' || name === 'fat' || name.includes('fat')) nutrients.fats = value;
										// Cholesterol
										if (name === 'cholesterol') nutrients.cholesterol = value;
										// Carbs
										if (name === 'carbohydrate, by difference' || name === 'carbohydrate' || name.includes('carbohydrate')) nutrients.carbs = value;
										// Sugar
										if (name === 'sugars, total including nleap' || name === 'sugar' || name.includes('sugar')) nutrients.sugar = value;
										// Fiber
										if (name === 'fiber, total dietary' || name === 'fiber' || name.includes('fiber')) nutrients.fiber = value;
										// Sodium
										if (name === 'sodium, na' || name === 'sodium' || name.includes('sodium')) nutrients.sodium = value;
									});
									// Fallback: try alternate fields if main ones are missing
									if (!nutrients.calories && detail.labelNutrients && detail.labelNutrients.calories) nutrients.calories = detail.labelNutrients.calories.value;
									if (!nutrients.protein && detail.labelNutrients && detail.labelNutrients.protein) nutrients.protein = detail.labelNutrients.protein.value;
									if (!nutrients.fats && detail.labelNutrients && detail.labelNutrients.fat) nutrients.fats = detail.labelNutrients.fat.value;
									if (!nutrients.carbs && detail.labelNutrients && detail.labelNutrients.carbohydrates) nutrients.carbs = detail.labelNutrients.carbohydrates.value;
									if (!nutrients.sugar && detail.labelNutrients && detail.labelNutrients.sugars) nutrients.sugar = detail.labelNutrients.sugars.value;
									if (!nutrients.fiber && detail.labelNutrients && detail.labelNutrients.dietaryFiber) nutrients.fiber = detail.labelNutrients.dietaryFiber.value;
									if (!nutrients.sodium && detail.labelNutrients && detail.labelNutrients.sodium) nutrients.sodium = detail.labelNutrients.sodium.value;
								}
								var foodItem = {
									foodName: foodName,
									id: fdcId,
									calories: nutrients.calories || 0,
									protein: nutrients.protein || 0,
									fats: nutrients.fats || 0,
									cholesterol: nutrients.cholesterol || 0,
									carbs: nutrients.carbs || 0,
									sugar: nutrients.sugar || 0,
									fiber: nutrients.fiber || 0,
									sodium: nutrients.sodium || 0
								};
								function showValue(val, unit) {
									if (val === undefined || val === null || val === "") return "0" + (unit || "");
									if (Number(val) === 0) return "0" + (unit || "");
									return Math.round(Number(val)) + (unit || "");
								}
								var resultHtml = '<div class="mb-2">'
									+ '<strong>' + foodItem.foodName + '</strong> (FDC ID: <span class="text-primary">' + foodItem.id + '</span>)<br>'
									+ 'Calories: ' + showValue(foodItem.calories, ' kcal') + '<br>'
									+ 'Protein: ' + showValue(foodItem.protein, 'g') + '<br>'
									+ 'Fats: ' + showValue(foodItem.fats, 'g') + '<br>'
									+ 'Cholesterol: ' + showValue(foodItem.cholesterol, 'mg') + '<br>'
									+ 'Carbs: ' + showValue(foodItem.carbs, 'g') + '<br>'
									+ 'Sugar: ' + showValue(foodItem.sugar, 'g') + '<br>'
									+ 'Fiber: ' + showValue(foodItem.fiber, 'g') + '<br>'
									+ 'Sodium: ' + showValue(foodItem.sodium, 'mg') + '<br>'
									+ '</div>'
									+ '<button id="append-food-btn" class="btn btn-success">Append to Food List</button>';
								$('#food-search-results').html(resultHtml);
								$('#append-food-btn').on('click', function() {
									var username = localStorage.getItem('calcal_last_user');
									if (!username) {
										$('#food-search-results').append('<div class="text-danger">Please sign in first.</div>');
										return;
									}
									$.ajax({
										url: 'http://127.0.0.1:5000/add_food',
										method: 'POST',
										contentType: 'application/json',
										data: JSON.stringify({ username: username, food: foodItem }),
										success: function(res) {
											if (res.success) {
												$('#food-search-results').append('<div class="text-success">Food added!</div>');
												setTimeout(function() { location.reload(true); }, 800);
											} else {
												$('#food-search-results').append('<div class="text-danger">' + (res.error || 'Failed to add food.') + '</div>');
											}
										},
										error: function(xhr) {
											$('#food-search-results').append('<div class="text-danger">Failed to add food.</div>');
										}
									});
								});
							},
							error: function() {
								$('#food-search-results').html('<span class="text-danger">Failed to get food details from USDA.</span>');
							}
						});
					} else {
						$('#food-search-results').html('<span class="text-danger">Food not found in USDA database.</span>');
					}
				},
				error: function() {
					$('#food-search-results').html('<span class="text-danger">Error searching USDA database.</span>');
				}
			});
		});
	}
	// Delete food item logic
	if ($('#food-delete-form').length) {
		$('#food-delete-form').on('submit', function(e) {
			e.preventDefault();
			var username = localStorage.getItem('calcal_last_user');
			var deleteName = $('#delete-food-name').val().trim();
			if (!username || !deleteName) {
				$('#delete-message').css('color', 'red').text('Please enter a food name to delete.');
				return;
			}
			$.ajax({
				url: 'http://127.0.0.1:5000/delete_food',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, foodName: deleteName }),
				success: function(res) {
					if (res.success) {
						$('#delete-message').css('color', 'green').text('Food item deleted successfully!');
						setTimeout(function() {
							location.reload(true);
						}, 800);
					} else {
						$('#delete-message').css('color', 'red').text(res.error || 'Failed to delete food item.');
					}
				},
				error: function(xhr) {
					if (xhr.responseJSON && xhr.responseJSON.error) {
						$('#delete-message').css('color', 'red').text(xhr.responseJSON.error);
					} else {
						$('#delete-message').css('color', 'red').text('Failed to delete food item.');
					}
				}
			});
			$('#delete-food-name').val('');
		});
	}
	// Render daily intake donut bars in right column
	if ($('#donut-bars').length) {
		var username = localStorage.getItem('calcal_last_user');
		var foodListKey = 'calcal_foods_' + username;
		var foodList = JSON.parse(localStorage.getItem(foodListKey) || '[]');
		// Sum daily totals and round immediately
		var totals = {
			calories: 0, protein: 0, fats: 0, cholesterol: 0, carbs: 0, sugar: 0, fiber: 0, sodium: 0
		};
		foodList.forEach(function(item) {
			totals.calories += Number(item.calories) || 0;
			totals.protein += Number(item.protein) || 0;
			totals.fats += Number(item.fats) || 0;
			totals.cholesterol += Number(item.cholesterol) || 0;
			totals.carbs += Number(item.carbs) || 0;
			totals.sugar += Number(item.sugar) || 0;
			totals.fiber += Number(item.fiber) || 0;
			totals.sodium += Number(item.sodium) || 0;
		});
		totals = roundTotals(totals);
		// Use user's nutritional goals for bar max, but show unmultiplied goal as denominator
		var users = JSON.parse(localStorage.getItem('calcal_users') || '{}');
		var goals = (users[username] || {}).nutritionalGoals || {};
		var barMaxVals = {
			calories: Math.round((Number(goals.caloriesValue) || 2000) * 1.5),
			protein: Math.round((Number(goals.proteinValue) || 50) * 1.5),
			fats: Math.round((Number(goals.fatsValue) || 78) * 1.5),
			cholesterol: Math.round((Number(goals.cholesterolValue) || 300) * 1.5),
			carbs: Math.round((Number(goals.carbsValue) || 275) * 1.5),
			sugar: Math.round((Number(goals.sugarValue) || 50) * 1.5),
			fiber: Math.round((Number(goals.fiberValue) || 28) * 1.5),
			sodium: Math.round((Number(goals.sodiumValue) || 2300) * 1.5)
		};
		var goalVals = {
			calories: Math.round(Number(goals.caloriesValue) || 2000),
			protein: Math.round(Number(goals.proteinValue) || 50),
			fats: Math.round(Number(goals.fatsValue) || 78),
			cholesterol: Math.round(Number(goals.cholesterolValue) || 300),
			carbs: Math.round(Number(goals.carbsValue) || 275),
			sugar: Math.round(Number(goals.sugarValue) || 50),
			fiber: Math.round(Number(goals.fiberValue) || 28),
			sodium: Math.round(Number(goals.sodiumValue) || 2300)
		};
		var labels = {
			calories: 'Calories', protein: 'Protein', fats: 'Fats', cholesterol: 'Cholesterol', carbs: 'Carbs', sugar: 'Sugar', fiber: 'Fiber', sodium: 'Sodium'
		};
		var donutHtml = '';
		Object.keys(totals).forEach(function(key) {
			var value = totals[key];
			var barMax = barMaxVals[key];
			var goal = goalVals[key];
			var percent = Math.min(100, Math.round((value / barMax) * 100));
			var color = '';
			// BAD DATA: Calories, Fats, Cholesterol, Sugar, Sodium, Carbs
			// GOOD DATA: Protein, Fiber
			if (["calories","fats","cholesterol","sugar","sodium","carbs"].includes(key)) {
				if (percent >= 67) { color = '#dc3545'; } // red
				else if (percent >= 34) { color = '#ffc107'; } // yellow
				else { color = '#198754'; } // green
			} else {
				if (percent >= 67) { color = '#198754'; } // green
				else if (percent >= 34) { color = '#ffc107'; } // yellow
				else { color = '#dc3545'; } // red
			}
			var canvasId = 'donut-' + key;
			donutHtml += '<div class="text-center">'
				+ '<canvas id="' + canvasId + '" width="120" height="120"></canvas>'
				+ '<div class="fw-semibold mt-2">' + labels[key] + '</div>'
				+ '<div class="small">' + value + ' / ' + goal + '</div>'
				+ '</div>';
		});
		$('#donut-bars').html(donutHtml);
		// Draw donuts
		Object.keys(totals).forEach(function(key) {
			var value = totals[key];
			var barMax = barMaxVals[key];
			var percent = Math.min(100, Math.round((value / barMax) * 100));
			// Determine color based on status (UNDER/OVER)
			var status = (goals[key + 'Type'] || 'under').toUpperCase();
			var color = '';
			if (status === 'UNDER') {
				if (percent >= 67) { color = '#dc3545'; } // High: Red
				else if (percent >= 34) { color = '#ffc107'; } // Moderate: Yellow
				else { color = '#198754'; } // Low: Green
			} else if (status === 'OVER') {
				if (percent >= 67) { color = '#198754'; } // High: Green
				else if (percent >= 34) { color = '#ffc107'; } // Moderate: Yellow
				else { color = '#dc3545'; } // Low: Red
			} else {
				color = '#0dcaf0'; // fallback
			}
			var canvas = document.getElementById('donut-' + key);
			if (canvas && canvas.getContext) {
				var ctx = canvas.getContext('2d');
				// Draw background circle
				ctx.clearRect(0, 0, 120, 120);
				ctx.beginPath();
				ctx.arc(60, 60, 50, 0, 2 * Math.PI);
				ctx.strokeStyle = '#e9ecef';
				ctx.lineWidth = 16;
				ctx.stroke();
				// Draw value arc
				ctx.beginPath();
				ctx.arc(60, 60, 50, -0.5 * Math.PI, (percent / 100) * 2 * Math.PI - 0.5 * Math.PI);
				ctx.strokeStyle = color;
				ctx.lineWidth = 16;
				ctx.stroke();
			}
		});
	}
	// Load food list from backend (homepage)
	function fetchFoodListAndRender() {
		var username = localStorage.getItem('calcal_last_user');
		if (!username) return;
		$.ajax({
			url: 'http://127.0.0.1:5000/get_foods',
			method: 'POST',
			contentType: 'application/json',
			data: JSON.stringify({ username: username }),
			success: function(res) {
				if (res.success && res.foods) {
					renderFoodListAndDonuts(res.foods);
				}
			}
		});
	}
	if ($('#food-list').length) {
		fetchFoodListAndRender();
	}
	// Food entry form logic (homepage)
	if ($('#food-entry-form').length) {
		$('#food-entry-form').on('submit', function(e) {
			e.preventDefault();
			var username = localStorage.getItem('calcal_last_user');
			if (!username) return;
			// Get values
			var foodItem = {
				foodName: $('#food-name').val().trim(),
				calories: $('#calories').val(),
				protein: $('#protein').val(),
				fats: $('#fats').val(),
				cholesterol: $('#cholesterol').val(),
				carbs: $('#carbs').val(),
				sugar: $('#sugar').val(),
				fiber: $('#fiber'
				).val(),
				sodium: $('#sodium').val()
			};
			$.ajax({
				url: 'http://127.0.0.1:5000/add_food',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, food: foodItem }),
				success: function(res) {
					if (res.success) {
						fetchFoodListAndRender();
						$('#food-entry-form')[0].reset();
					}
				}
			});
		});
	}
	if ($('#signin-form').length) {
		$('#login-btn').on('click', function(e) {
			e.preventDefault();
			var username = $('#login-username').val().trim();
			var password = $('#login-password').val();
			if (!username || !password) {
				$('#login-message').css('color', 'red').text('Please enter both username and password.');
				return;
			}
			$.ajax({
				url: 'http://127.0.0.1:5000/login',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, password: password }),
				success: function(res) {
					if (res.success) {
						localStorage.setItem('calcal_last_user', username);
						$('#login-message').css('color', 'green').text('Login successful! Redirecting...');
						setTimeout(function() {
							window.location.href = 'calcal_home.html';
						}, 1000);
					} else {
						$('#login-message').css('color', 'red').text('Invalid username or password.');
					}
				},
				error: function(xhr) {
					if (xhr.responseJSON && xhr.responseJSON.error) {
						$('#login-message').css('color', 'red').text(xhr.responseJSON.error);
					} else {
						$('#login-message').css('color', 'red').text('Login failed. Please try again.');
					}
				}
			});
		});
		$('#signup-btn').on('click', function() {
			window.location.href = 'calcal_usersetup.html';
		});
	}
});
$(document).ready(function() {
	// If on user setup page, run setup logic
	if ($('#user-setup-form').length) {
		// Dynamic unit switching
		$('#units').on('change', function() {
			var unit = $(this).val();
			var weight = parseFloat($('#weight').val());
			var height = parseFloat($('#height').val());
			// Only convert if there is a value entered
			if (!isNaN(weight)) {
				if (unit === 'imperial' && $('#weight-label').text().includes('kg')) {
					// kg to lb
					weight = Math.round(weight * 2.20462 * 100) / 100;
					$('#weight').val(weight);
				} else if (unit === 'metric' && $('#weight-label').text().includes('lb')) {
					// lb to kg
					weight = Math.round(weight / 2.20462 * 100) / 100;
					$('#weight').val(weight);
				}
			}
			if (!isNaN(height)) {
				if (unit === 'imperial' && $('#height-label').text().includes('cm')) {
					// cm to in
					height = Math.round(height / 2.54 * 100) / 100;
					$('#height').val(height);
				} else if (unit === 'metric' && $('#height-label').text().includes('in')) {
					// in to cm
					height = Math.round(height * 2.54 * 100) / 100;
					$('#height').val(height);
				}
			}
			if (unit === 'imperial') {
				$('#weight-label').text('Weight (lb):');
				$('#height-label').text('Height (in):');
			} else {
				$('#weight-label').text('Weight (kg):');
				$('#height-label').text('Height (cm):');
			}
		});

		// Enable/disable submit button based on validation
		function validateForm() {
			var username = $('#username').val().trim();
			var password = $('#password').val();
			var weight = $('#weight').val();
			var height = $('#height').val();
			var age = $('#age').val();
			var gender = $('#gender').val();
			var activity = $('#activity').val();
			var units = $('#units').val();
			var allFilled = username && password && weight && height && age && gender && activity && units;
			var strongRegex = /^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)(?=.*[!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?]).{8,}$/;
			var strongPassword = strongRegex.test(password);
			if (!allFilled) {
				$('#form-message').css('color', 'red').text('Please fill out all fields.');
				$('#submit-btn').prop('disabled', true);
				return;
			}
			if (!strongPassword) {
				$('#form-message').css('color', 'red').text('Password must be at least 8 characters and include uppercase, lowercase, number, and special character.');
				$('#submit-btn').prop('disabled', true);
				return;
			}
			// Check username uniqueness via backend
			if (username) {
				$.ajax({
					url: 'http://127.0.0.1:5000/check_username',
					method: 'POST',
					contentType: 'application/json',
					data: JSON.stringify({ username: username }),
					success: function(res) {
						if (res.exists) {
							$('#form-message').css('color', 'red').text('Username already exists. Please choose another.');
							$('#submit-btn').prop('disabled', true);
						} else {
							$('#form-message').text('');
							$('#submit-btn').prop('disabled', false);
						}
					},
					error: function() {
						$('#form-message').css('color', 'red').text('Could not validate username.');
						$('#submit-btn').prop('disabled', true);
					}
				});
			}
		}

		$('#user-setup-form input, #user-setup-form select').on('input change', validateForm);

		// User setup form submission
		$('#user-setup-form').on('submit', function(e) {
			e.preventDefault();
			var username = $('#username').val().trim();
			var password = $('#password').val();
			var weight = $('#weight').val();
			var height = $('#height').val();
			var age = $('#age').val();
			var gender = $('#gender').val();
			var activity = $('#activity').val();
			var units = $('#units').val();

			// Register user in backend
			$.ajax({
				url: 'http://127.0.0.1:5000/register',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username, password: password }),
				success: function(res) {
					if (res.success) {
						// Save additional user info to backend settings
						$.ajax({
							url: 'http://127.0.0.1:5000/update_settings',
							method: 'POST',
							contentType: 'application/json',
							data: JSON.stringify({
								username: username,
								weight: weight,
								height: height,
								age: age,
								gender: gender,
								activity: activity,
								units: units
							}),
							success: function() {
								localStorage.setItem('calcal_last_user', username);
								$('#form-message').css('color', 'green').text('User setup saved successfully!');
								setTimeout(function() {
									window.location.href = 'calcal_home.html';
								}, 1000);
							},
							error: function() {
								$('#form-message').css('color', 'red').text('Failed to save user settings.');
							}
						});
					} else {
						$('#form-message').css('color', 'red').text(res.error || 'Registration failed.');
					}
				},
				error: function(xhr) {
					if (xhr.responseJSON && xhr.responseJSON.error) {
						$('#form-message').css('color', 'red').text(xhr.responseJSON.error);
					} else {
						$('#form-message').css('color', 'red').text('Registration failed.');
					}
				}
			});
			this.reset();
			$('#submit-btn').prop('disabled', true);
		});

		// Initial validation
		validateForm();
	}

	// If on homepage, display user info
	if ($('#profile-details').length) {
		// ...existing code...
		var username = localStorage.getItem('calcal_last_user');
		if (username) {
			$.ajax({
				url: 'http://127.0.0.1:5000/get_settings',
				method: 'POST',
				contentType: 'application/json',
				data: JSON.stringify({ username: username }),
				success: function(res) {
					var user = res.settings || {};
					$('#welcome-username').text('Welcome, ' + username + '!');
					var html = '';
					html += '<div><strong>Username:</strong> ' + username + '</div>';
					html += '<div><strong>Weight:</strong> ' + (user.weight || '') + '</div>';
					html += '<div><strong>Height:</strong> ' + (user.height || '') + '</div>';
					html += '<div><strong>Age:</strong> ' + (user.age || '') + '</div>';
					html += '<div><strong>Gender:</strong> ' + (user.gender || '') + '</div>';
					html += '<div><strong>Activity:</strong> ' + (user.activity || '') + '</div>';
					html += '<div><strong>Units:</strong> ' + (user.units || '') + '</div>';
					$('#profile-details').html(html);
				},
				error: function() {
					$('#profile-details').html('<div class="text-danger">Failed to load user info.</div>');
				}
			});
		} else {
			$('#profile-details').html('<div class="text-danger">No user signed in.</div>');
		}
	}
});