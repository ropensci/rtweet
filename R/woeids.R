#' find_woeid
#'
#' @description Returns WOEID number for desired town or country.
#'
#' @param x Character string, place
#' @keywords internal
#' @export
find_woeid <- function(x) {
	woeid <- NULL

	if (length(woeid_towns(x)) == 1) {
		woeid <- woeid_countries(x)
	}

	if (length(woeid_towns(x)) == 1) {
		if (!is.null(woeid)) {
			woeid <- c(woeid, woeid_towns(x))
		} else {
			woeid <- woeid_towns(x)
		}
	}
	woeid
}

#' check_woeid
#'
#' @description Returns WOEID number for desired town or country.
#'
#' @param x Character string, place
#' @keywords internal
#' @export
check_woeid <- function(x) {
	woeid <- suppressWarnings(as.numeric(x))
	if (is.na(woeid)) {
		woeid <- find_woeid(x)
	}
	stopifnot(is.numeric(woeid))
	woeid
}

woeid_countries <- function(x) {
  names <- c("United Arab Emirates", "Algeria", "Argentina",
    "Australia", "Austria", "Bahrain", "Belgium", "Belarus",
    "Brazil", "Canada", "Chile", "Colombia", "Denmark", "Dominican Republic",
    "Ecuador", "Egypt", "Ireland", "France", "Ghana", "Germany",
    "Greece", "Guatemala", "Indonesia", "India", "Israel",
    "Italy", "Japan", "Jordan", "Kenya", "Korea", "Kuwait",
    "Lebanon", "Latvia", "Oman", "Mexico", "Malaysia", "Nigeria",
    "Netherlands", "Norway", "New Zealand", "Peru", "Pakistan",
    "Poland", "Panama", "Portugal", "Qatar", "Philippines",
    "Puerto Rico", "Russia", "Saudi Arabia", "South Africa",
    "Singapore", "Spain", "Sweden", "Switzerland", "Thailand",
    "Turkey", "United Kingdom", "Ukraine", "United States",
    "Venezuela", "Vietnam")

  woeid <- c(23424738, 23424740, 23424747, 23424748, 23424750,
    23424753, 23424757, 23424765, 23424768, 23424775, 23424782,
    23424787, 23424796, 23424800, 23424801, 23424802, 23424803,
    23424819, 23424824, 23424829, 23424833, 23424834, 23424846,
    23424848, 23424852, 23424853, 23424856, 23424860, 23424863,
    23424868, 23424870, 23424873, 23424874, 23424898, 23424900,
    23424901, 23424908, 23424909, 23424910, 23424916, 23424919,
    23424922, 23424923, 23424924, 23424925, 23424930, 23424934,
    23424935, 23424936, 23424938, 23424942, 23424948, 23424950,
    23424954, 23424957, 23424960, 23424969, 23424975, 23424976,
    23424977, 23424982, 23424984)

  if (!tolower(x) %in% tolower(names)) {
  	if (length(grep(x, names, ignore.case = TRUE, value = TRUE)) == 1) {
  		x <- grep(x, names, ignore.case = TRUE, value = TRUE)
  	}
  }

  woeid[which(tolower(names) == tolower(x))]
}

woeid_towns <- function(x) {

  names <- c(
  	"Winnipeg", "Ottawa", "Quebec", "Montreal", "Toronto",
    "Edmonton", "Calgary", "Vancouver", "Birmingham", "Blackpool",
    "Bournemouth", "Brighton", "Bristol", "Cardiff", "Coventry",
    "Derby", "Edinburgh", "Glasgow", "Hull", "Leeds", "Leicester",
    "Liverpool", "Manchester", "Middlesbrough", "Newcastle",
    "Nottingham", "Plymouth", "Portsmouth", "Preston", "Sheffield",
    "Stoke-on-Trent", "Swansea", "London", "Belfast", "Santo Domingo",
    "Guatemala City", "Acapulco", "Aguascalientes", "Chihuahua",
    "Mexico City", "Ciudad Juarez", "Nezahualcóyotl", "Culiacán",
    "Ecatepec de Morelos", "Guadalajara", "Hermosillo", "León",
    "Mérida", "Mexicali", "Monterrey", "Morelia", "Naucalpan de Juárez",
    "Puebla", "Querétaro", "Saltillo", "San Luis Potosí",
    "Tijuana", "Toluca", "Zapopan", "Mendoza", "Santiago",
    "Concepcion", "Valparaiso", "Bogotá", "Cali", "Medellín",
    "Barranquilla", "Quito", "Guayaquil", "Caracas", "Maracaibo",
    "Maracay", "Valencia", "Barcelona", "Ciudad Guayana",
    "Turmero", "Lima", "Brasília", "Belém", "Belo Horizonte",
    "Curitiba", "Porto Alegre", "Recife", "Rio de Janeiro",
    "Salvador", "São Paulo", "Campinas", "Fortaleza", "Goiânia",
    "Manaus", "São Luís", "Guarulhos", "Córdoba", "Rosario",
    "Barquisimeto", "Maturín", "Buenos Aires", "Gdańsk",
    "Kraków", "Lodz", "Poznań", "Warsaw", "Wroclaw", "Vienna",
    "Cork", "Dublin", "Galway", "Bordeaux", "Lille", "Lyon",
    "Marseille", "Montpellier", "Nantes", "Paris", "Rennes",
    "Strasbourg", "Toulouse", "Berlin", "Bremen", "Dortmund",
    "Dresden", "Dusseldorf", "Essen", "Frankfurt", "Hamburg",
    "Cologne", "Leipzig", "Munich", "Stuttgart", "Bologna",
    "Genoa", "Milan", "Naples", "Palermo", "Rome", "Turin",
    "Den Haag", "Amsterdam", "Rotterdam", "Utrecht", "Barcelona",
    "Bilbao", "Las Palmas", "Madrid", "Malaga", "Murcia",
    "Palma", "Seville", "Valencia", "Zaragoza", "Geneva",
    "Lausanne", "Zurich", "Brest", "Grodno", "Gomel", "Minsk",
    "Riga", "Bergen", "Oslo", "Gothenburg", "Stockholm",
    "Dnipropetrovsk", "Donetsk", "Kharkiv", "Kyiv", "Lviv",
    "Odesa", "Zaporozhye", "Athens", "Thessaloniki", "Bekasi",
    "Depok", "Pekanbaru", "Surabaya", "Makassar", "Bandung",
    "Jakarta", "Medan", "Palembang", "Semarang", "Tangerang",
    "Singapore", "Perth", "Adelaide", "Brisbane", "Canberra",
    "Darwin", "Melbourne", "Sydney", "Kitakyushu", "Saitama",
    "Chiba", "Fukuoka", "Hamamatsu", "Hiroshima", "Kawasaki",
    "Kobe", "Kumamoto", "Nagoya", "Niigata", "Sagamihara",
    "Sapporo", "Sendai", "Takamatsu", "Tokyo", "Yokohama",
    "Goyang", "Yongin", "Ansan", "Bucheon", "Busan", "Changwon",
    "Daegu", "Gwangju", "Incheon", "Seongnam", "Suwon", "Ulsan",
    "Seoul", "Kajang", "Ipoh", "Johor Bahru", "Klang", "Kuala Lumpur",
    "Calocan", "Makati", "Pasig", "Taguig", "Antipolo", "Cagayan de Oro",
    "Cebu City", "Davao City", "Manila", "Quezon City", "Zamboanga City",
    "Bangkok", "Hanoi", "Hai Phong", "Can Tho", "Da Nang",
    "Ho Chi Minh City", "Algiers", "Accra", "Kumasi", "Benin City",
    "Ibadan", "Kaduna", "Kano", "Lagos", "Port Harcourt",
    "Giza", "Cairo", "Alexandria", "Mombasa", "Nairobi",
    "Durban", "Johannesburg", "Port Elizabeth", "Pretoria",
    "Cape Town", "Medina", "Dammam", "Riyadh", "Jeddah",
    "Mecca", "Sharjah", "Abu Dhabi", "Dubai", "Haifa", "Tel Aviv",
    "Jerusalem", "Amman", "Chelyabinsk", "Khabarovsk", "Krasnodar",
    "Krasnoyarsk", "Samara", "Voronezh", "Yekaterinburg",
    "Irkutsk", "Kazan", "Moscow", "Nizhny Novgorod", "Novosibirsk",
    "Omsk", "Perm", "Rostov-on-Don", "Saint Petersburg",
    "Ufa", "Vladivostok", "Volgograd", "Karachi", "Lahore",
    "Multan", "Rawalpindi", "Faisalabad", "Muscat", "Nagpur",
    "Lucknow", "Kanpur", "Patna", "Ranchi", "Kolkata", "Srinagar",
    "Amritsar", "Jaipur", "Ahmedabad", "Rajkot", "Surat",
    "Bhopal", "Indore", "Thane", "Mumbai", "Pune", "Hyderabad",
    "Bangalore", "Chennai", "Mersin", "Adana", "Ankara",
    "Antalya", "Bursa", "Diyarbakır", "Eskişehir", "Gaziantep",
    "Istanbul", "Izmir", "Kayseri", "Konya", "Okinawa", "Daejeon",
    "Auckland", "Albuquerque", "Atlanta", "Austin", "Baltimore",
    "Baton Rouge", "Birmingham", "Boston", "Charlotte", "Chicago",
    "Cincinnati", "Cleveland", "Colorado Springs", "Columbus",
    "Dallas-Ft. Worth", "Denver", "Detroit", "El Paso", "Fresno",
    "Greensboro", "Harrisburg", "Honolulu", "Houston", "Indianapolis",
    "Jackson", "Jacksonville", "Kansas City", "Las Vegas",
    "Long Beach", "Los Angeles", "Louisville", "Memphis",
    "Mesa", "Miami", "Milwaukee", "Minneapolis", "Nashville",
    "New Haven", "New Orleans", "New York", "Norfolk", "Oklahoma City",
    "Omaha", "Orlando", "Philadelphia", "Phoenix", "Pittsburgh",
    "Portland", "Providence", "Raleigh", "Richmond", "Sacramento",
    "St. Louis", "Salt Lake City", "San Antonio", "San Diego",
    "San Francisco", "San Jose", "Seattle", "Tallahassee",
    "Tampa", "Tucson", "Virginia Beach", "Washington", "Osaka",
    "Kyoto", "Delhi", "Petaling", "Hulu Langat", "Okayama")

  woeid <- c(2972, 3369, 3444, 3534, 4118, 8676, 8775, 9807,
    12723, 12903, 13383, 13911, 13963, 15127, 17044, 18114,
    19344, 21125, 25211, 26042, 26062, 26734, 28218, 28869,
    30079, 30720, 32185, 32452, 32566, 34503, 36240, 36758,
    44418, 44544, 76456, 83123, 110978, 111579, 115958, 116545,
    116556, 116564, 117994, 118466, 124162, 124785, 131068,
    133327, 133475, 134047, 134091, 134395, 137612, 138045,
    141272, 144265, 149361, 149769, 151582, 332471, 349859,
    349860, 349861, 368148, 368149, 368150, 368151, 375732,
    375733, 395269, 395270, 395271, 395272, 395273, 395275,
    395277, 418440, 455819, 455820, 455821, 455822, 455823,
    455824, 455825, 455826, 455827, 455828, 455830, 455831,
    455833, 455834, 455867, 466861, 466862, 468382, 468384,
    468739, 493417, 502075, 505120, 514048, 523920, 526363,
    551801, 560472, 560743, 560912, 580778, 608105, 609125,
    610264, 612977, 613858, 615702, 619163, 627791, 628886,
    638242, 641142, 645458, 645686, 646099, 648820, 650272,
    656958, 667931, 671072, 676757, 698064, 711080, 716085,
    718345, 719258, 719846, 721943, 725003, 726874, 727232,
    733075, 734047, 753692, 754542, 764814, 766273, 766356,
    768026, 769293, 774508, 776688, 779063, 782538, 783058,
    784794, 824382, 825848, 825978, 834463, 854823, 857105,
    862592, 890869, 906057, 918981, 919163, 922137, 924938,
    924943, 929398, 939628, 946738, 963291, 1030077, 1032539,
    1040779, 1044316, 1046138, 1047180, 1047378, 1047908,
    1048059, 1048324, 1048536, 1062617, 1098081, 1099805,
    1100661, 1100968, 1101597, 1103816, 1105779, 1110809,
    1116753, 1117034, 1117099, 1117155, 1117227, 1117502,
    1117545, 1117605, 1117817, 1117881, 1118072, 1118108,
    1118129, 1118285, 1118370, 1118550, 1130853, 1132094,
    1132444, 1132445, 1132447, 1132449, 1132466, 1132481,
    1132496, 1132559, 1132567, 1132578, 1132599, 1141268,
    1154679, 1154698, 1154726, 1154781, 1167715, 1180689,
    1187115, 1195098, 1198785, 1199002, 1199079, 1199136,
    1199477, 1199682, 1199980, 1225448, 1236594, 1236690,
    1252351, 1252376, 1252431, 1253079, 1326075, 1330595,
    1387660, 1393672, 1396439, 1396803, 1398823, 1404447,
    1521643, 1521894, 1522006, 1528335, 1528488, 1580913,
    1582504, 1586614, 1586638, 1591691, 1937801, 1939574,
    1939753, 1939873, 1939897, 1940119, 1940330, 1940345,
    1967449, 1968212, 1968222, 1968902, 1997422, 2018708,
    2028717, 2029043, 2077746, 2108210, 2112237, 2121040,
    2121267, 2122265, 2122471, 2122541, 2122641, 2122814,
    2123177, 2123260, 2124045, 2124288, 2124298, 2211096,
    2211177, 2211269, 2211387, 2211574, 2268284, 2282863,
    2295377, 2295378, 2295381, 2295383, 2295386, 2295387,
    2295388, 2295401, 2295402, 2295404, 2295405, 2295407,
    2295408, 2295410, 2295411, 2295412, 2295414, 2295420,
    2295424, 2323778, 2343678, 2343732, 2343733, 2343843,
    2343932, 2343980, 2343999, 2344116, 2344117, 2344174,
    2344210, 2345896, 2345975, 2348079, 2352824, 2357024,
    2357536, 2358820, 2359991, 2364559, 2367105, 2378426,
    2379574, 2380358, 2381475, 2383489, 2383660, 2388929,
    2391279, 2391585, 2397816, 2407517, 2414469, 2418046,
    2423945, 2424766, 2427032, 2428184, 2428344, 2430683,
    2436704, 2441472, 2442047, 2442327, 2449323, 2449808,
    2450022, 2451822, 2452078, 2457170, 2458410, 2458833,
    2459115, 2460389, 2464592, 2465512, 2466256, 2471217,
    2471390, 2473224, 2475687, 2477058, 2478307, 2480894,
    2486340, 2486982, 2487610, 2487796, 2487889, 2487956,
    2488042, 2490383, 2503713, 2503863, 2508428, 2512636,
    2514815, 15015370, 15015372, 20070458, 56013632, 56013645,
    90036018)

  if (!tolower(x) %in% tolower(names)) {
  	if (length(grep(x, names, ignore.case = TRUE, value = TRUE)) == 1) {
  		x <- grep(x, names, ignore.case = TRUE, value = TRUE)
  	}
  }

  woeid[which(tolower(names) == tolower(x))]
}
