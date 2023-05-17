from winnower.custom.base import TransformBase
import pandas
import numpy
import math
from datetime import datetime

class Transform(TransformBase):

	def output_columns(self, input_columns):
		print("\n-----------------------------",flush=True)
		print("\nEXTRACTING UbCov ID#: ",self.config['ubcov_id'],flush=True)

		res = input_columns + ['measurement', 'hemoglobin_raw', 'hemoglobin_alt_adj','hemog_alt_smoke_adjust','brinda_adj_hemog','pregnant_orig',
		'cv_pregnant','cv_smoke','smoking_number','cluster_altitude_unit','child_dob',
		'anemia_mild_raw','anemia_moderate_raw','anemia_severe_raw','anemia_anemic_raw',
		'anemia_mild_adj','anemia_moderate_adj','anemia_severe_adj','anemia_anemic_adj',
		'anemia_mild_brinda','anemia_moderate_brinda','anemia_severe_brinda','anemia_anemic_brinda',
		'trimester','last_preg_days_ago','last_menstrual_period_days','cv_lactating']

		return res


	def execute(self,df):
		df = self._rename_cols(df)
		df = self._update_sex_id(df)
		df = self._update_age_year(df)
		df = self._keep_hemog_obs(df)
		df = self._sift_hemog(df)
		df = self._get_pregnancy_status(df)
		df = self._get_smoking_status(df)
		df = self._get_smoking_freq(df)
		df = self._final_operations(df)
		return df

	#################################################
	# RENAME/PREP DATA SECTION

	"""
	updates col names to be consistent with codebooking and updates any original stata col names with our codebook names
	"""
	def _rename_cols(self,df):
		print("\nRENAMING COLUMNS",flush=True)
		col_names = df.columns #get the current column names that the winnower script compiled
		config_names = self.config #get the columns names from the codebook (aka the top row of the codebook)
		for col in config_names: #sift through all of the codebook column names
			if col not in col_names: #if the column we codebooked has not shown up in the df
				old_col_name = self.config[col] #get the actual column name that was codebooked
				if old_col_name in df and "hgb_val" not in col: #custom for anemia -- if it has "hgb_val_*" in the name, don't switch column name
					print("\t"+old_col_name+" TO "+col,flush=True)
					df.rename(columns={old_col_name : col},inplace=True) #replace the column name in the df
				elif isinstance(old_col_name,str) and old_col_name.upper() in df and "hgb_val" not in col: #in case the .dta file has upper case column names
					print("\t"+old_col_name+" TO "+col,flush=True)
					df.rename(columns={old_col_name.upper() : col},inplace=True) #replace the column name in the df

		if 'birth_date' in df and 'child_dob' not in df:
			print("\tbirth_date TO child_dob",flush=True)
			df.rename(columns={'birth_date' : 'child_dob'},inplace=True) #replace the column name in the df
		elif 'birth_date' in df and 'child_dob' in df:
			del df['birth_date']
			print("\tDropping birth_date column (do not need for this extraction)",flush=True)

		if 'hh_id' in df and 'atomic_hh_id' not in df:
			print("\thh_id TO atomic_hh_id",flush=True)
			df.rename(columns={'hh_id' : 'atomic_hh_id'},inplace=True) #replace the column name in the df
		elif 'hh_id' in df and 'atomic_hh_id' not in df:
			print("\tDropping hh_id column (not needed for this extraction)",flush=True)
			del df['hh_id']

		return df

	#################################################
	# SEX STATUS ASSIGNMENT SECTION

	"""
	update the sex id to be 3 in the case the value is blank
	"""
	def _update_sex_id(self,df):
		print("\nUPDATING SEX ID",flush=True) #update user it is in update_sex_id function
		update_needed = False
		if 'sex_id' in df:
			selector = (df.sex_id.isna()) & (df.sex_id!=1) & (df.sex_id!=2)
			if len(df[selector].index)>0:
				df.loc[selector,'sex_id'] = 3 #update it to be 3 (both M and F)
				print("\tWARNING: Some sex_id vars not set, defaulting to 3. Please check survey and codebook and rerun!",flush=True)
				update_needed = True
		else:
			df['sex_id'] = 3 #if sex_id has not yet been codebooked, default everything to both M and F
			print("\tWARNING: NO sex_id vars set, defaulting to 3. Please check survey and codebook and rerun!",flush=True)
			update_needed = True
		if self.config['survey_module']=="WN":
			df.loc[df.sex_id==3,'sex_id'] = 2
			if update_needed:
				print("\tUPDATE: given that it was a women's survey module, all sex_id that was set to 3 has been set to 2.",flush=True)
		elif self.config['survey_module']=="MN":
			df.loc[df.sex_id==3,'sex_id'] = 1
			if update_needed:
				print("\tUPDATE: given that it was a men's survey module, all sex_id that was set to 3 has been set to 1.",flush=True)
		return df


	#################################################
	# AGE ASSIGNMENT SECTION

	"""
	Some surveys have children under-5 (age_month < 60 OR age_day < 1826) being given age_year=5
	"""
	def _update_age_year(self,df):
		print("\nUPDATING AGE YEAR",flush=True)
		if 'age_month' in df: #if age month is in df
			print("\tUPDATE: age_year updated to reflect age_month values.", flush=True)
			selector = (df.age_month<=60) & (df.age_year<=5) #update age year for all children under 5 years old
			df.loc[selector,'age_year'] = df.age_month/12
		if 'age_day' in df: #if age day is in the df
			print("\tUPDATE: age_year updated to reflect age_day values.", flush=True)
			selector = (df.age_day<=365) & (df.age_year<=1) #update age year for all children under 1 years old
			df.loc[selector,'age_year'] = df.age_day/365
		if 'age_year' not in df:
			print("\tWARNING: NO age_year vars set. Please check survey and codebook and rerun!",flush=True)
		elif 'age_year' in df and 'age_sampling_lower' in df:
			if 'age_month' not in df and 'age_day' not in df and float(self.config['age_sampling_lower'])>=.5: #if age_year is in the df but not age_month and age_day, but the min age is >0.5 years old
				df.loc[df.age_year<1,'age_year'] = .5 #update everything that is below 1 years old to be 6 months old since the 6-11 month age range all have the same anemia thresholds
			elif 'age_month' in df and 'age_day' not in df and float(self.config['age_sampling_lower'])<.083334:
				print("\tWARNING: minimum age denoted in codebook as <1 month, but no age_day present in survey to confirm granularity of age below 1 month. Check output post extraction and impute age later based on age-split mean hemog levels.",flush=True)
			elif 'age_month' not in df and 'age_day' not in df and float(self.config['age_sampling_lower'])<.5:
				print("\tWARNING: minimum age denoted in codebooke as <6 months, but no age_month or age_day to confirm granularity of age below 6 months. Check output post extraction and impute age later based on age-split mean hemog levels.",flush=True)
		return df

	#################################################
	# HEMOG KEEP/OMIT SECTION based on cv_* flags

	"""
	locate the measured cols where hgb_measured==0 or 1 (aka contains hemog data)
	"""
	def _keep_hemog_obs(self,df):
		print("\nKEEP HEMOG OBS",flush=True) #update user it is in keep_hemog_obs function
		df.loc['measurement'] = float('nan') #initialize a column that contains only zeros
		cols_2_check = ['hgb_result_female','hgb_result_male','hgb_result_child'] #columns to check
		for col in cols_2_check:
			if col in df:
				print("\t"+col,flush=True)
				df.loc[df[col].notna(),'measurement'] = 0 #if column exists, set all values where the column values exist to 0 in the 'measurement' column
				selector = (df[col]== int(self.config['hgb_measured'])) #check to see if there are any columns that have the measured flag
				df.loc[selector,'measurement'] = 1 #set those columns equal to 1 if they do exist
		if 'measurement' in df:
			df = df[df.measurement==1].copy() #only keep the rows that contain measurements, use .copy() to get rid of warning message and allow all contents of DF to be retained
		else:
			print('\tNo hemog indicator flag - no observations dropped.',flush=True)
		return df

	#################################################
	# HEMOGLOBIN SIFTING SECTION

	"""
	function that compiles all of the hemog data
	"""
	def _compile_hemog_data(self,df,codebook_col_name,new_col_name,cols_2_check,temp_col_name):
		print("\tChecking to see if hemog values are present for: "+codebook_col_name,flush=True)
		if codebook_col_name in self.config and int(self.config[codebook_col_name]) == 1: #if there are hemog values present
			if temp_col_name != 'smoke_alt': #only check for children duplicate column names if we are not checking for adjusted smoking/altitude values
				if self.config[cols_2_check[0]] == self.config[cols_2_check[2]]:#drop child hemog reading column if columns labels are the same in the codebook 
					if cols_2_check[2] in df:		
						del df[cols_2_check[2]]
					numpy.delete(cols_2_check,2) #drop it from the cols_2_check vector so we can speed up the parsing process
			if self.config[cols_2_check[0]] == self.config[cols_2_check[1]]:#drop male hemog reading column if columns labels are the same in the codebook 
				if cols_2_check[1] in df:	
					del df[cols_2_check[1]]
				numpy.delete(cols_2_check,1) #drop it from the cols_2_check vector so we can speed up the parsing process

			print("\t\tGenerating new column: "+new_col_name,flush=True)
			df[new_col_name] = float('nan') #make a placeholder for raw hemog vals
			for col in cols_2_check:
				print("\t\tChecking codebook column: "+col,flush=True)
				if col in df: #if the column still exists in the df and wasn't dropped earlier
					num_blanks = df[col].isna().sum() #count the number of empty values
					num_rows = len(df.index)
					if num_rows==num_blanks: #if that column exists but didn't contain any values, stop extraction
						error_msg = "\t\t\tNo hemog data in column: "+col+". Check codebook and survey to confirm! Dropping column."
						print(error_msg,flush=True)
						del df[col]
					else:
						if self.config['hgb_unit']!="":
							df.loc[df[col].notna(),new_col_name] = df.loc[df[col].notna(),col] 
							if(self.config['hgb_unit'].lower()=="g/dl"):
								df[new_col_name] = df[new_col_name]*10 #normalize all data to be in g/L
							del df[col]
						else:
							error_msg = "\t\t\tWARNING: NO HEMOG UNIT SPECIFIED. Update codebook and rerun!"
							print(error_msg,flush=True)
							
		else:
			print("\t\tNo hemog data for: "+codebook_col_name+". Check codebook and survey to confirm! Dropping all columns associated with: "+codebook_col_name,flush=True)
			for col in cols_2_check:
				if col in df:
					print(col,flush=True)
					del df[col] #get rid of all columns that were orginially in the codebook

		return df

	"""
	function that sifts through all of the hemog-reading cols
	"""
	def _sift_hemog(self,df):
		print("\nSIFTING THROUGH HEMOG VALUES",flush=True)
		hemog_flag_vec = ['cv_hgb_raw','cv_hgb_altitude','cv_hgb_altitude_smoking'] #all of the codebook columns that have T/F flag if data is present for these categories
		new_col_vec = ['hemoglobin_raw','hemoglobin_alt_adj','hemog_alt_smoke_adjust'] #new columns name that will be present in codebook post extraction
		codebook_cols = pandas.DataFrame({'raw':['hgb_female_raw','hgb_male_raw','hgb_child_raw'], #all of the codebook columns that have data for hemog readings
			'alt':['hgb_female_altitude','hgb_male_altitude','hgb_child_altitude'],
			'smoke_alt':['hgb_female_altitude_smoking','hgb_male_altitude_smoking','hgb_child_altitude_smoking']})
		codebook_col_names = codebook_cols.columns #get the column names for the each category of hemog data
		for i in range(len(hemog_flag_vec)): #iterate through each category to extract hemog data
			curr_cols_vec = codebook_cols[codebook_col_names[i]].values
			df = self._compile_hemog_data(df,hemog_flag_vec[i],new_col_vec[i],curr_cols_vec,codebook_col_names[i])

		#now sift through the newly extracted data to check and see if any data that made it into the df should be removed based on the parameters below
		print("\tNumber of rows in DF pre pruning: "+str(len(df.index)),flush=True)
		selector = None
		for col in new_col_vec:
			if col in df:
				if selector is None:
					selector = (df[col]>=25) & (df[col]<=210) 
				else:
					selector = selector | (df[col]>=25) & (df[col]<=210)

		df = df[selector].copy() #only keep values that meet the params above
		print("\tNumber of rows in DF post pruning: "+str(len(df.index)),flush=True)

		return df

	#################################################
	# PREGNANCY & TRIMESTER STATUS ASSIGNMENT SECTION

	"""
	function that calculates the trimester of a pregnancy
	"""
	def _calculate_trimester(self,df):
		selector = (~numpy.isnan(df.time_preg)) & (df.sex_id==2) & (df.age_year>=15) & (df.age_year<50) & (df.cv_pregnant==1) & (df.time_preg<=3)
		df.loc[selector,'trimester'] = 1
		selector = (~numpy.isnan(df.time_preg)) & (df.sex_id==2) & (df.age_year>=15) & (df.age_year<50) & (df.cv_pregnant==1) & (df.time_preg>=4) & (df.time_preg<=6)
		df.loc[selector,'trimester'] = 2
		selector = (~numpy.isnan(df.time_preg)) & (df.sex_id==2) & (df.age_year>=15) & (df.age_year<50) & (df.cv_pregnant==1) & (df.time_preg>=7) & (df.time_preg<=10)
		df.loc[selector,'trimester'] = 3
		return df

	"""
	function that converts mentrual period date in days
	#   last_menstrual_period is formatted in DHS surveys as 3 digit number, where first digit is denoting time in days, weeks, months, years:
	#   1 -> days ago
	#   2 -> weeks ago
	#   3 -> months ago
	#   4 -> years ago
	#   Example -> 302 = 2 months ago
	"""
	def _convert_menstrual_period_date(self,val):
		val = str(val)
		count_mult_vec = [1,7,30.45,364.25] #day, week, month, years all in days
		time_type = int(val[0])-1
		date_mult = count_mult_vec[time_type]
		days_ago = date_mult*float(val[1:])
		return int(days_ago)

	"""
	function that calculates last menstrual period into days
	"""
	def _calculate_menstrual_period_date(self,df):
		selector = (~numpy.isnan(df.last_menstrual_period)) & (df.sex_id==2) & (df.age_year>=15) & (df.age_year<50) & (df.last_menstrual_period < 500) & (df.last_menstrual_period > 0)
		df.loc[selector,'last_menstrual_period_days'] = df.loc[selector,'last_menstrual_period'].apply(self._convert_menstrual_period_date)
		return df

	"""
	function that gets pregnancy status of each women in the survey
	"""
	def _get_pregnancy_status(self,df):
		print("\nGETTING PREGNANCY STATUS",flush=True)
		if 'pregnant' in df:
			df['pregnant_orig'] = df['pregnant'] #copy over the pregnancy status column
			df['cv_pregnant'] = 0 #default pregnancy status to 0
			preg_tuple = self.config['pregnant_true'] #get the 'true' meta-values from the survey
			for tup in preg_tuple: #parse through the true values
				selector = (df['pregnant']==int(tup)) & (df['age_year']>=10) & (df['sex_id']==2) #if pregnancy value is present and true, if female, and >10 yo
				df.loc[selector,'cv_pregnant'] = 1 #then update pregnancy status to true!

			if 'time_preg' in df and 'age_year' in df and 'sex_id' in df:
				print("\tDetermining trimester",flush=True)
				df['trimester'] = 999
				df = self._calculate_trimester(df)
			else:
				print("\tNOT determining trimester",flush=True)

			if 'last_menstrual_period' in df:
				print("\tDetermining last menstrual period",flush=True)
				df['last_menstrual_period_days'] = float('nan')
				df = self._calculate_menstrual_period_date(df)
			else:
				print("\tNOT determining last menstrual period",flush=True)

		else:
			print("\tWARNING: no pregnancy variable. Check survey and codebook and rerun!",flush=True)
		return df


	#################################################
	# SMOKING STATUS ASSIGNMENT SECTION

	"""
	function that gets all smoking yes/no status in survey
	"""
	def _get_smoking_status(self,df):
		print("\nGETTING SMOKING STATUS",flush=True)
		if 'smoking_status' in df:
			df['cv_smoke'] = 0 #default smoking status status to 0
			smoke_tuple = self.config['smoking_status_true'] #get the 'true' meta-values from the survey
			for tup in smoke_tuple:
				selector = (df['smoking_status']==int(tup)) #find all smoking 'true' values
				df.loc[selector,'cv_smoke'] = 1 #and update our new column to true
		return df

	"""
	function that gets smoking frequency
	"""
	def _get_smoking_freq(self,df):
		print("\nGETTING SMOKING FREQUENCY",flush=True)
		mf_vec = ['female','male']
		for x in mf_vec:
			mf_smoking_number = x+"_smoking_number"
			if mf_smoking_number in df:
				if 'smoking_number' not in df:
					df['smoking_number'] = float('nan')
				mf_missing_number = x+"_smoking_number_missing"
				vec = self.config[mf_missing_number] #get the smoking values that were denoted as being no-nos
				for i in range(len(vec)): #for each "bad value"
					selector = None #make a place holder for the parameters
					if ">" in vec[i]:
						new_val = int(vec[i].split(">")[1]) #get the number after the value
						selector = (df[mf_smoking_number]<=new_val) #reverse the logic to keep the values that are within the reasonable bounds
					elif "<" in vec[i]:
						new_val = int(vec[i].split("<")[1]) #get the number after the value
						selector = (df[mf_smoking_number]>=new_val) #reverse the logic to keep the values that are within the reasonable bounds
					else:
						new_val = int(vec[i]) #convert the string to a number
						selector = (df[mf_smoking_number]!=new_val) #reverse the logic to keep the values that are within the reasonable bounds
					df.loc[selector,'smoking_number'] = df.loc[selector,mf_smoking_number]
				del df[mf_smoking_number]

		smoking_rate = None #placeholder for smoking rate
		if 'smoking_time_interval' in df: #if the smoking rate is in the df
			smoking_rate = self.config['smoking_time_interval'] #update the smoking rate value
			del df['smoking_time_interval'] #drop the smoking time interval column since it's duplicated info
		elif 'smoking_time_interval' in self.config: #if the smoking time interval is only in self.config
			smoking_rate = self.config['smoking_time_interval'] #update the smoking rate value

		if smoking_rate is not None:
			df['smoking_number'] = df['smoking_number']/int(smoking_rate)
		else:
			print("\tWARNING: Smoking rate is not defined. Please input into codebook and rerun.",flush=True)

		return df


	#################################################
	# ROW BY ROW CALCULATIONS for - BRINDA, LAST PREGNANCY & LACTATING STATUS, and ANEMIA STATUS ASSIGNMENT

	"""
	step 0 helper function - returns age group id #
	"""
	def _get_age_id(self,curr_age,age_df):
		for r in range(len(age_df.index)):
			if curr_age >= float(age_df.iloc[[r]]['age_group_years_start']) and curr_age < float(age_df.iloc[[r]]['age_group_years_end']):
				return int(age_df.iloc[[r]]['age_group_id'])
		return -1

	"""
	step 1 main function - calculates brinda_adj_hemog and returns the adjusted value
	"""
	def _calculate_brinda(self,df,r,sex_val,age_val,preg_val):
		brinda_adj_hemog = None
		dis_hemog = df.iloc[[r]]['hemoglobin_raw'].values[0]
		dis_altitude = df.iloc[[r]]['cluster_altitude'].values[0]
		if ~numpy.isnan(dis_hemog) and ~numpy.isnan(dis_altitude): #make sure they are numbers
			if sex_val==2 and age_val>=15 and age_val<50 and preg_val==0: #if WRA and not pregnant
				brinda_adj_hemog = int(dis_hemog) - (0.0052792*float(dis_altitude) + 0.0000004*float(dis_altitude)**2)
			elif age_val>=0.5 and age_val<5: #if PSC
				brinda_adj_hemog = int(dis_hemog) - (0.0048108*float(dis_altitude) + 0.0000003*float(dis_altitude)**2)
		return brinda_adj_hemog

	"""
	step 2.1 main function - determines date of last pregnancy by finding dob of youngest child
	"""
	def _find_date_of_last_preg(self,df,r):
		dis_hh = int(df.iloc[[r]]['atomic_hh_id']) #get the household ID
		child_id = df.iloc[[r]]['youngest_child_index'] #get the household line index of the youngest child
		dis_strata = int(df.iloc[[r]]['strata']) #get the strata that the household is in
		dis_psu = int(df.iloc[[r]]['psu']) #get the cluster the household is in
		dob = None
		if child_id is not None and ~numpy.isnan(float(child_id)): #if there is a youngest child
			child_id = int(child_id) #get the line index
			selector = (df.hh_member_index == child_id) & (df.atomic_hh_id == dis_hh) & (df.strata == dis_strata) & (df.psu == dis_psu) #find the row of the youngest child in the survey
			dob = df.loc[selector,'child_dob'].values #get the childs dob
			if len(dob) == 1: #if the there is an item there
				try:
					dob = int(dob[0]) #try to get the cmc dob
				except Exception as e:
					dob = None #else reset the dob to none
			else:
				dob = None
		return dob

	"""
	step 2.2 helper function - converts YYYY/MM/DD dates to date time
	"""
	def _convert_to_date_time(self,yr,month,day):
		toReturn = None
		if day is not None:
			try:
				toReturn = datetime(yr,month,day)
			except:
				toReturn = datetime(yr,month,15)
		else:
			toReturn = datetime(yr,month,15) #dhs defaults all dates without day to the 15th of the month
		return toReturn

	"""
	step 2.3 helper function - converts cmc date to YYYY/MM/DD dates
	# Starting date is January 1900
	# CMC = (YY*12) + MM -> use for years 1900 to 1999
	# CMC = ((YYYY-1900) * 12) + MM -> for years >= 2000
	# YYYY = int((CMC - 1) / 12)+1900 -> ”int” is equivalent to “floor” given numbers are > 0 (i.e. truncate decimals)
	# MM = CMC - ((YYYY-1900) * 12)
	"""
	def _convert_cmc_date(self,val):
		dis_year = math.floor((int(val)-1)/12)+1900
		dis_month = int(val) - ((dis_year-1900)*12)
		good_date = self._convert_to_date_time(dis_year,dis_month,15)
		return good_date

	"""
	function that goes row by row and defines: BRINDA, date of last pregnancy, and anemia status assignment
	"""
	def _final_operations(self,df):
		print("\nGETTING ANEMIA STATUS",flush=True)	
		#load in anemia levels file
		gbd_ages = pandas.read_csv("/share/mnch/anemia/code/reference/model/age_groups.csv")
		anemia_levels = pandas.read_csv("/share/mnch/anemia/code/reference/model/anemia_thresholds.csv")
		level_col_df = pandas.DataFrame({'lower':["hgb_lower_mild","hgb_lower_moderate","hgb_lower_severe","hgb_lower_anemic"],
			'upper':["hgb_upper_mild","hgb_upper_moderate","hgb_upper_severe","hgb_upper_anemic"]})

		skip_counter = 0 #a counter to index the amount of times we weren't able to assign anemia status for a given row

		#make new severity cols
		sev_cols = ['anemia_mild','anemia_moderate','anemia_severe','anemia_anemic']
		col_categories = ["_raw","_adj"]

		#this section of code checks to see if the survey has the proper parameters to be run through the BRINDA altitude equation
		brinda_flag = False #default to False
		#if cluster altitude is in the codebook AND df AND it has been defined AND it has raw hemoglobin readings 
		if 'cluster_altitude' in self.config and 'cluster_altitude' in df and self.config['cluster_altitude'] is not None and 'cv_hgb_raw' in df and int(self.config['cv_hgb_raw'])==1:
			print("\tCaclulating BRINDA altitude adjusted hemoglobin values",flush=True) #print a message that signifies that this survey will be run against BRINDA equations (given it meets the criteria below)
			df['brinda_adj_hemog'] = float('nan') #create a column where adjusted BRINDA hemog values will go
			col_categories.append("_brinda")
			brinda_flag = True #set brinda_flag to true
		else:
			print("\tNOT caclulating BRINDA altitude adjusted hemoglobin values",flush=True)

		lactate_flag = False
		if 'child_dob' in df and 'hh_member_index' in df and 'youngest_child_index' in df and 'int_year' in df and 'int_month' in df:
			lactate_flag = True
			df['last_preg_days_ago'] = float('nan')
			df['cv_lactating'] = 999
			print("\tDetermining lactating status",flush=True)
		else:
			print("\tNOT determining lactating status",flush=True)

		for col in sev_cols:
			for cat in col_categories:
				col_name = col+cat
				df[col_name] = 0

		for r in range(len(df.index)):

			# STEP 0 - get initial parameters on survey subject
			good2go = True
			try:
				dis_age = float(df.iloc[[r]]['age_year'])
				age_id = self._get_age_id(dis_age,gbd_ages)
			except Exception as e:
				good2go=False
				age_id = -1 #pass an invalid age id to not allow it to go through the anemia assignment loop
				print("\tWARNING: row "+str(r)+" doesn't have age_year defined. Skipping anemia status assignment.",flush=True)
				print(e)
			dis_sex = int(df.iloc[[r]]['sex_id'])
			dis_preg = 0
			if dis_sex==2 and dis_age>=10 and 'cv_pregnant' in df:
				dis_preg = int(df.iloc[[r]]['cv_pregnant'])

			# STEP 1 - calculate brinda adjusted hemog vals, if able to do so!
			brinda_adj_hemog = None
			if brinda_flag:
				brinda_adj_hemog = self._calculate_brinda(df,r,dis_sex,dis_age,dis_preg)
				if brinda_adj_hemog is not None:
					df.iat[r,df.columns.get_loc('brinda_adj_hemog')]=brinda_adj_hemog #insert it into the df

			# STEP 2 - calculate date of last pregnancy
			if lactate_flag and dis_sex==2 and dis_age>=15 and dis_age<50 and dis_preg==0:
				child_dob = self._find_date_of_last_preg(df,r)
				if child_dob is not None:
					child_dob_dt = self._convert_cmc_date(child_dob)
					int_year = int(df.iloc[[r]]['int_year'])
					int_month = int(df.iloc[[r]]['int_month'])
					int_day = None
					if 'int_day' in df and ~numpy.isnan(float(df.iloc[[r]]['int_day'])):
						int_day = int(df.iloc[[r]]['int_day'])
					interview_dt = self._convert_to_date_time(int_year,int_month,int_day)
					day_diff = interview_dt - child_dob_dt
					day_diff = day_diff.days
					df.iat[r,df.columns.get_loc('last_preg_days_ago')]=day_diff #insert it into the df
					if day_diff < 61:
						df.iat[r,df.columns.get_loc('cv_lactating')] = 2
					elif day_diff >= 61 and day_diff <= 365:
						df.iat[r,df.columns.get_loc('cv_lactating')] = 1
					else:
						df.iat[r,df.columns.get_loc('cv_lactating')] = 0


			# STEP 3 - anemia status assignment
			selector = (anemia_levels['age_group_id']==age_id) & (anemia_levels['sex_id']==dis_sex) & (anemia_levels['pregnant']==dis_preg)
			temp_anemia = anemia_levels[selector] #get the row with the correct hemog bounds
			
			if len(temp_anemia.index)==1 and good2go:
				
				if brinda_flag and brinda_adj_hemog is not None: #if a brinda hemoglobin value was recorded
					for x in range(len(level_col_df.index)): #now assign it an anemia status (if applicable)
						col_name_lower = level_col_df.iloc[[x]]['lower'].values
						col_name_upper = level_col_df.iloc[[x]]['upper'].values
						lower_bound = temp_anemia.iloc[[0]][col_name_lower[0]].values
						upper_bound = temp_anemia.iloc[[0]][col_name_upper[0]].values
						if brinda_adj_hemog >= int(lower_bound[0]) and brinda_adj_hemog < int(upper_bound[0]):
							col_name = sev_cols[x]+"_brinda"
							df.iat[r,df.columns.get_loc(col_name)]=1
							df.iat[r,df.columns.get_loc('anemia_anemic_brinda')]=1
							break
						else:
							col_name = sev_cols[x]+"_brinda"
							df.iat[r,df.columns.get_loc(col_name)]=0
							df.iat[r,df.columns.get_loc('anemia_anemic_brinda')]=0

				
				#get raw hemog status
				if int(self.config['cv_hgb_raw'])==1:
					dis_hemog = df.iloc[[r]]['hemoglobin_raw'].values
					dis_hemog = dis_hemog[0]
					if ~numpy.isnan(dis_hemog):
						dis_hemog = int(dis_hemog)
						for x in range(len(level_col_df.index)):
							col_name_lower = level_col_df.iloc[[x]]['lower'].values
							col_name_upper = level_col_df.iloc[[x]]['upper'].values
							lower_bound = temp_anemia.iloc[[0]][col_name_lower[0]].values
							upper_bound = temp_anemia.iloc[[0]][col_name_upper[0]].values
							if dis_hemog >= int(lower_bound[0]) and dis_hemog < int(upper_bound[0]):
								col_name = sev_cols[x]+"_raw"
								df.iat[r,df.columns.get_loc(col_name)]=1
								df.iat[r,df.columns.get_loc('anemia_anemic_raw')]=1
								break

				#get raw hemog status
				if int(self.config['cv_hgb_altitude_smoking'])==1:
					dis_hemog = df.iloc[[r]]['hemog_alt_smoke_adjust'].values
					dis_hemog = dis_hemog[0]			
					if ~numpy.isnan(dis_hemog):
						dis_hemog = int(dis_hemog)	
						for x in range(len(level_col_df.index)):
							col_name_lower = level_col_df.iloc[[x]]['lower'].values
							col_name_upper = level_col_df.iloc[[x]]['upper'].values
							lower_bound = temp_anemia.iloc[[0]][col_name_lower[0]].values
							upper_bound = temp_anemia.iloc[[0]][col_name_upper[0]].values
							if dis_hemog >= int(lower_bound[0]) and dis_hemog < int(upper_bound[0]):
								col_name = sev_cols[x]+"_adj"
								df.iat[r,df.columns.get_loc(col_name)]=1
								df.iat[r,df.columns.get_loc('anemia_anemic_adj')]=1
								break

				if int(self.config['cv_hgb_altitude'])==1:
					dis_hemog = df.iloc[[r]]['hemoglobin_alt_adj'].values
					dis_hemog = dis_hemog[0]
					if ~numpy.isnan(dis_hemog):
						dis_hemog = int(dis_hemog)
						for x in range(len(level_col_df.index)):
							col_name_lower = level_col_df.iloc[[x]]['lower'].values
							col_name_upper = level_col_df.iloc[[x]]['upper'].values
							lower_bound = temp_anemia.iloc[[0]][col_name_lower[0]].values
							upper_bound = temp_anemia.iloc[[0]][col_name_upper[0]].values
							if dis_hemog >= int(lower_bound[0]) and dis_hemog < int(upper_bound[0]):
								col_name = sev_cols[x]+"_adj"
								df.iat[r,df.columns.get_loc(col_name)]=1
								df.iat[r,df.columns.get_loc('anemia_anemic_adj')]=1
								break
			else:
				skip_counter+=1

		if skip_counter>0:
			print("\t"+str(skip_counter)+" out of "+str(len(df.index))+" rows were skipped do to either: sex_id==3 OR no age_year present OR unkown pregnancy status (check errors shown above). Check codebook, original survey, or output file and rerun if needed!\n",flush=True)
		else:
			print("\tSUCCESS: All rows were assigned an anemia status!\n",flush=True)

		print("Number of rows in final DF: "+str(len(df.index)),flush=True)
		return df

	


