# Classification
Classification Problem - Identify defective waterpoints in developing countries

Water is one of the most basic human needs. Of late, we have been witnessing acute water shortages across countries and it is starting to become a critical problem in modern society. Governments spend millions in their attempt to ensure water availability across cities, towns and villages. Of all the water sources available, one source which stands out in ease of access as well as quality is ground water. In an urban setting, ground water is generally pulled via powerful pumps and stored in tanks. However, in rural areas wells and handpumps are the most accessible options. Governments install hand pumps in villages to provide for the villagers. However, often there are no regular maintenance checks of the pumps; partly due to high operational cost involved in travel and technicians, the maintenance of the hand pumps is generally ignored by the authorities.

In this data challenge, we ask:
Is there a way we can use predictive modelling techniques to predict whether a given handpump(read waterpoint) is defective? This would greatly help plan visits of technicians to places where problem exists instead of planning to go everywhere. There would be two direct benefits of this: 
1.	Huge cost savings 
2.	Early problem detection and resolution
3.	Most importantly, people won’t have to wait indefinitely for help!

You have been provided with data collected across various villages in rural Africa. A lot of information has been captured related to the location, usage and attributes of the installed waterpoints. Given the features of an installed waterpoint, your goal is to classify a given water point as defective or not. Note that waterpoint includes hand pump, stand pipe, cattle trough  etc.

Data dictionary - the features in this data set are:

•	amount_tsh - Total static head (amount water available to waterpoint)

•	date_recorded - The date the row was entered

•	funder - Who funded the well

•	gps_height - Altitude of the well

•	installer - Organization that installed the well

•	longitude - GPS coordinate

•	latitude - GPS coordinate

•	wpt_name - Name of the waterpoint if there is one

•	num_private - No information available

•	basin - Geographic water basin

•	subvillage - Geographic location

•	region - Geographic location

•	region_code - Geographic location (coded)

•	district_code - Geographic location (coded)

•	lga - Geographic location

•	ward - Geographic location

•	population - Population around the well

•	public_meeting - True/False

•	recorded_by - Group entering this row of data

•	scheme_management - Who operates the waterpoint

•	scheme_name - Who operates the waterpoint

•	permit - If the waterpoint is permitted

•	construction_year - Year the waterpoint was constructed

•	extraction_type - The kind of extraction the waterpoint uses

•	extraction_type_group - The kind of extraction the waterpoint uses

•	extraction_type_class - The kind of extraction the waterpoint uses

•	management - How the waterpoint is managed

•	management_group - How the waterpoint is managed

•	payment - What the water costs

•	payment_type - What the water costs

•	water_quality - The quality of the water

•	quality_group - The quality of the water

•	quantity - The quantity of water

•	quantity_group - The quantity of water

•	source - The source of the water

•	source_type - The source of the water

•	source_class - The source of the water

•	waterpoint_type - The kind of waterpoint (handpump, communal standpipe, cattle trough etc.)

•	waterpoint_type_group - The kind of waterpoint

Labels (column name: Defective):

•	yes – the waterpoint is nonfunctional – this is the positive class in this problem

•	no – the waterpoint is functional and there are no repairs needed

If a working waterpoint is wrongly classified as faulty (false positive), the cost involved might include a technician visit and inspection; however, if a non-functional waterpoint is classified as functional (false negative), then there is no action taken, and there is consequently a significant social cost. Hence, there is a much higher penalty for false positives.
