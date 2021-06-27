let noms =
[|
"existence";
"inexistence";
"substantiality";
"unsubstantiality";
"intrinsicality";
"extrinsicality";
"state";
"circumstance";
"relation";
"irrelation";
"consanguinity";
"correlation";
"identity";
"contrariety";
"difference";
"uniformity";
"non-uniformity";
"similarity";
"dissimilarity";
"imitation";
"non-imitation";
"variation";
"copy";
"prototype";
"agreement";
"disagreement";
"quantity";
"degree";
"equality";
"inequality";
"mean";
"compensation";
"greatness";
"smallness";
"superiority";
"inferiority";
"increase";
"decrease";
"addition";
"subduction";
"adjunct";
"remainder";
"decrement";
"mixture";
"simpleness";
"junction";
"disjunction";
"vinculum";
"coherence";
"incoherence";
"combination";
"decomposition";
"whole";
"part";
"completeness";
"incompleteness";
"composition";
"omission";
"component";
"extraneousness";
"order";
"disorder";
"arrangement";
"derangement";
"precedence";
"sequence";
"precursor";
"sequel";
"beginning";
"end";
"middle";
"continuity";
"discontinuity";
"term";
"assemblage";
"dispersion";
"focus";
"class";
"inclusion";
"exclusion";
"generality";
"speciality";
"rule";
"multiformity";
"conformity";
"unconformity";
"number";
"numeration";
"list";
"unity";
"accompaniment";
"duality";
"duplication";
"bisection";
"triality";
"triplication";
"trisection";
"quaternity";
"quadruplication";
"quadrisection";
"five or more";
"quinquesection or finer";
"plurality";
"fraction";
"zero";
"multitude";
"fewness";
"repetition";
"infinity";
"time";
"neverness";
"period";
"contingent duration";
"course";
"diuturnity";
"transientness";
"perpetuity";
"instantaneity";
"chronometry";
"anachronism";
"priority";
"posteriority";
"present time";
"different time";
"synchronism";
"futurity";
"preterition";
"newness";
"oldness";
"morning";
"evening";
"youth";
"age";
"infant";
"veteran";
"adolescence";
"earliness";
"lateness";
"occasion";
"intempestivity";
"frequency";
"infrequency";
"periodicity";
"irregularity";
"change";
"permanence";
"cessation";
"continuance";
"conversion";
"reversion";
"revolution";
"substitution";
"interchange";
"changeableness";
"stability";
"eventuality";
"destiny";
"cause";
"effect";
"attribution";
"chance";
"power";
"impotence";
"strength";
"weakness";
"production";
"destruction";
"reproduction";
"producer";
"destroyer";
"paternity";
"posterity";
"productiveness";
"unproductiveness";
"agency";
"energy";
"inertness";
"violence";
"moderation";
"influence";
"absence of influence";
"tendency";
"liability";
"concurrence";
"counteraction";
"space";
"inextension";
"region";
"place";
"situation";
"location";
"displacement";
"presence";
"absence";
"inhabitant";
"abode";
"contents";
"receptacle";
"size";
"littleness";
"expansion";
"contraction";
"distance";
"nearness";
"interval";
"contiguity";
"length";
"shortness";
"thickness";
"thinness";
"layer";
"filament";
"height";
"lowness";
"depth";
"shallowness";
"summit";
"base";
"verticality";
"horizontality";
"pendency";
"support";
"parallelism";
"obliquity";
"inversion";
"crossing";
"exteriority";
"interiority";
"centrality";
"covering";
"lining";
"investment";
"divestment";
"circumjacence";
"interjacence";
"circumscription";
"outline";
"edge";
"inclosure";
"limit";
"front";
"rear";
"laterality";
"contraposition";
"dextrality";
"sinistrality";
"form";
"amorphism";
"symmetry";
"distortion";
"angularity";
"curvature";
"straightness";
"circularity";
"convolution";
"rotundity";
"convexity";
"flatness";
"concavity";
"sharpness";
"bluntness";
"smoothness";
"roughness";
"notch";
"fold";
"furrow";
"opening";
"closure";
"perforator";
"stopper";
"motion";
"quiescence";
"journey";
"navigation";
"traveller";
"mariner";
"transference";
"carrier";
"vehicle";
"ship";
"velocity";
"slowness";
"impulse";
"recoil";
"direction";
"deviation";
"precession";
"following";
"progression";
"regression";
"propulsion";
"traction";
"approach";
"recession";
"attraction";
"repulsion";
"convergence";
"divergence";
"arrival";
"departure";
"ingress";
"egress";
"reception";
"ejection";
"food";
"excretion";
"insertion";
"extraction";
"passage";
"transcursion";
"shortcoming";
"ascent";
"descent";
"elevation";
"depression";
"leap";
"plunge";
"circuition";
"rotation";
"evolution";
"oscillation";
"agitation";
"materiality";
"immateriality";
"world";
"gravity";
"levity";
"density";
"rarity";
"hardness";
"softness";
"elasticity";
"inelasticity";
"tenacity";
"brittleness";
"texture";
"pulverulence";
"friction";
"lubrication";
"fluidity";
"gaseity";
"liquefaction";
"vaporization";
"water";
"air";
"moisture";
"dryness";
"ocean";
"land";
"gulf";
"plain";
"marsh";
"island";
"stream";
"river";
"wind";
"conduit";
"air-pipe";
"semiliquidity";
"bubble";
"pulpiness";
"unctuousness";
"oil";
"resin";
"organization";
"inorganization";
"life";
"death";
"killing";
"corpse";
"interment";
"animality";
"vegetability";
"animal";
"vegetable";
"zoology";
"botany";
"cicuration";
"agriculture";
"mankind";
"man";
"woman";
"physical sensibility";
"physical insensibility";
"physical pleasure";
"physical pain";
"touch";
"sensations of touch";
"numbness";
"heat";
"cold";
"calefaction";
"refrigeration";
"furnace";
"refrigeratory";
"fuel";
"thermometer";
"taste";
"insipidity";
"pungency";
"condiment";
"savouriness";
"unsavouriness";
"sweetness";
"sourness";
"odour";
"inodorousness";
"fragrance";
"fetor";
"sound";
"silence";
"loudness";
"faintness";
"snap";
"roll";
"resonance";
"non-resonance";
"sibilation";
"stridor";
"cry";
"ululation";
"melody";
"discord";
"music";
"musician";
"musical instruments";
"hearing";
"deafness";
"light";
"darkness";
"dimness";
"luminary";
"shade";
"transparency";
"opacity";
"semitransparency";
"colour";
"achromatism";
"whiteness";
"blackness";
"grayness";
"brownness";
"redness";
"greenness";
"yellowness";
"purpleness";
"blueness";
"orangeness";
"variegation";
"vision";
"blindness";
"dimsightedness";
"spectator";
"optical instruments";
"visibility";
"invisibility";
"appearance";
"disappearance";
"intellect";
"absence of intellect";
"thought";
"incogitancy";
"idea";
"topic";
"curiosity";
"incuriosity";
"attention";
"inattention";
"care";
"neglect";
"inquiry";
"answer";
"experiment";
"comparison";
"discrimination";
"indiscrimination";
"measurement";
"evidence";
"counter-evidence";
"qualification";
"possibility";
"impossibility";
"probability";
"improbability";
"certainty";
"uncertainty";
"reasoning";
"intuition";
"demonstration";
"confutation";
"judgment";
"discovery";
"misjudgment";
"overestimation";
"underestimation";
"belief";
"unbelief";
"credulity";
"incredulity";
"assent";
"dissent";
"knowledge";
"ignorance";
"scholar";
"ignoramus";
"truth";
"error";
"maxim";
"absurdity";
"wisdom";
"folly";
"sage";
"fool";
"sanity";
"insanity";
"madman";
"memory";
"oblivion";
"expectation";
"inexpectation";
"disappointment";
"foresight";
"prediction";
"omen";
"oracle";
"supposition";
"imagination";
"meaning";
"unmeaningness";
"intelligibility";
"unintelligibility";
"equivocalness";
"metaphor";
"interpretation";
"misinterpretation";
"interpreter";
"manifestation";
"latency";
"information";
"concealment";
"disclosure";
"ambush";
"publication";
"news";
"secret";
"messenger";
"affirmation";
"negation";
"teaching";
"misteaching";
"learning";
"teacher";
"learner";
"school";
"veracity";
"falsehood";
"deception";
"untruth";
"dupe";
"deceiver";
"exaggeration";
"indication";
"record";
"obliteration";
"recorder";
"representation";
"misrepresentation";
"painting";
"sculpture";
"engraving";
"artist";
"language";
"letter";
"word";
"neology";
"nomenclature";
"misnomer";
"phrase";
"grammar";
"solecism";
"style";
"perspicuity";
"obscurity";
"conciseness";
"diffuseness";
"vigor";
"feebleness";
"plainness";
"ornament";
"elegance";
"inelegance";
"voice";
"aphony";
"speech";
"stammering";
"loquacity";
"taciturnity";
"allocution";
"response";
"interlocution";
"soliloquy";
"writing";
"printing";
"correspondence";
"book";
"description";
"dissertation";
"compendium";
"poetry";
"prose";
"drama";
"will";
"necessity";
"willingness";
"unwillingness";
"resolution";
"perseverance";
"irresolution";
"obstinacy";
"tergiversation";
"caprice";
"choice";
"absence of choice";
"rejection";
"predetermination";
"spontaneity";
"habit";
"desuetude";
"motive";
"absence of motive";
"dissuasion";
"plea";
"good";
"evil";
"intention";
"non-design";
"pursuit";
"avoidance";
"relinquishment";
"business";
"plan";
"method";
"mid-course";
"circuit";
"requirement";
"instrumentality";
"means";
"instrument";
"substitute";
"materials";
"store";
"provision";
"waste";
"sufficiency";
"insufficiency";
"redundance";
"importance";
"unimportance";
"utility";
"inutility";
"expedience";
"inexpedience";
"goodness";
"badness";
"perfection";
"imperfection";
"cleanness";
"uncleanness";
"health";
"disease";
"salubrity";
"insalubrity";
"improvement";
"deterioration";
"restoration";
"relapse";
"remedy";
"bane";
"safety";
"danger";
"refuge";
"pitfall";
"warning";
"alarm";
"preservation";
"escape";
"deliverance";
"preparation";
"non-preparation";
"essay";
"undertaking";
"use";
"disuse";
"misuse";
"action";
"inaction";
"activity";
"inactivity";
"haste";
"leisure";
"exertion";
"repose";
"fatigue";
"refreshment";
"agent";
"workshop";
"conduct";
"management";
"director";
"advice";
"council";
"precept";
"skill";
"unskilfulness";
"proficient";
"bungler";
"cunning";
"artlessness";
"difficulty";
"facility";
"hindrance";
"aid";
"opposition";
"cooperation";
"opponent";
"auxiliary";
"party";
"dissension";
"concord";
"defiance";
"attack";
"defence";
"retaliation";
"resistance";
"contention";
"peace";
"warfare";
"pacification";
"mediation";
"submission";
"combatant";
"arms";
"arena";
"completion";
"non-completion";
"success";
"failure";
"trophy";
"prosperity";
"adversity";
"mediocrity";
"authority";
"laxity";
"severity";
"lenity";
"command";
"disobedience";
"obedience";
"compulsion";
"master";
"servant";
"sceptre";
"freedom";
"subjection";
"liberation";
"restraint";
"prison";
"keeper";
"prisoner";
"commission";
"abrogation";
"resignation";
"consignee";
"deputy";
"permission";
"prohibition";
"consent";
"offer";
"refusal";
"request";
"deprecation";
"petitioner";
"promise";
"release";
"compact";
"conditions";
"security";
"observance";
"non-observance";
"compromise";
"acquisition";
"loss";
"possession";
"exemption";
"participation";
"possessor";
"property";
"retention";
"non-retention";
"transfer";
"giving";
"receiving";
"apportionment";
"lending";
"borrowing";
"taking";
"restitution";
"stealing";
"thief";
"booty";
"barter";
"purchase";
"sale";
"merchant";
"merchandise";
"mart";
"money";
"treasurer";
"treasury";
"wealth";
"poverty";
"credit";
"debt";
"payment";
"non-payment";
"expenditure";
"receipt";
"accounts";
"price";
"discount";
"dearness";
"cheapness";
"liberality";
"economy";
"prodigality";
"parsimony";
"affections";
"feeling";
"sensibility";
"insensibility";
"excitation";
"excitability";
"inexcitability";
"pleasure";
"pain";
"pleasurableness";
"painfulness";
"content";
"discontent";
"regret";
"relief";
"aggravation";
"cheerfulness";
"dejection";
"rejoicing";
"lamentation";
"amusement";
"weariness";
"wit";
"dulness";
"humorist";
"beauty";
"ugliness";
"ornamentation";
"blemish";
"simplicity";
"good taste";
"vulgarity";
"fashion";
"ridiculousness";
"fop";
"affectation";
"ridicule";
"laughing-stock";
"hope";
"hopelessness";
"fear";
"courage";
"cowardice";
"rashness";
"caution";
"desire";
"indifference";
"dislike";
"fastidiousness";
"satiety";
"wonder";
"expectance";
"prodigy";
"repute";
"disrepute";
"nobility";
"commonality";
"title";
"pride";
"humility";
"vanity";
"modesty";
"ostentation";
"celebration";
"boasting";
"insolence";
"servility";
"blusterer";
"friendship";
"enmity";
"friend";
"enemy";
"sociality";
"seclusion";
"courtesy";
"discourtesy";
"congratulation";
"love";
"hate";
"favourite";
"resentment";
"irascibility";
"sullenness";
"endearment";
"marriage";
"celibacy";
"divorce";
"benevolence";
"malevolence";
"malediction";
"threat";
"philanthropy";
"misanthropy";
"benefactor";
"evil doer";
"pity";
"pitilessness";
"condolence";
"gratitude";
"ingratitude";
"forgiveness";
"revenge";
"jealousy";
"envy";
"right";
"wrong";
"dueness";
"undueness";
"duty";
"dereliction";
"non-ownership";
"respect";
"disrespect";
"contempt";
"approbation";
"disapprobation";
"flattery";
"detraction";
"flatterer";
"detractor";
"vindication";
"accusation";
"probity";
"improbity";
"knave";
"disinterestedness";
"selfishness";
"virtue";
"vice";
"innocence";
"guilt";
"good man";
"bad man";
"penitence";
"impenitence";
"atonement";
"temperance";
"intemperance";
"sensualist";
"asceticism";
"fasting";
"gluttony";
"sobriety";
"drunkenness";
"purity";
"impurity";
"libertine";
"legality";
"illegality";
"jurisdiction";
"tribunal";
"judge";
"lawyer";
"lawsuit";
"acquittal";
"condemnation";
"punishment";
"reward";
"penalty";
"scourge";
"deity";
"angel";
"satan";
"jupiter";
"demon";
"heaven";
"hell";
"theology";
"orthodoxy";
"heterodoxy";
"revelation";
"pseudo-revelation";
"piety";
"impiety";
"irreligion";
"worship";
"idolatry";
"sorcery";
"spell";
"sorcerer";
"churchdom";
"clergy";
"laity";
"rite";
"canonicals";
"temple";
|];;