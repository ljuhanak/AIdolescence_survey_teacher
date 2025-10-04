# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)
library(gsheet)

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = FALSE)



# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {

  url_params <- reactive({
    
    sd_get_url_pars()
    
  })
  

  observe({
    
    pars <- url_params()
    
    if (!is.null(pars["IDs"])) {
      sd_store_value(value = pars$IDs, id = "ID_skoly")
    }
    
    
    # Vektor ID AI otázek náhodně zamíchaný
    question_id <- sample(c("AIq1","AIq2","AIq3","AIq4","AIq5",
                            "AIq6","AIq7","AIq8","AIq9","AIq10",
                            "AIq11","AIq12","AIq13","AIq14","AIq15",
                            "AIq16","AIq17","AIq18","AIq19","AIq20",
                            "AIq21","AIq22","AIq23","AIq24","AIq25",
                            "AIq26","AIq27","AIq28","AIq29"))
    
    # Pomocná funkce na zamíchání pořadí možností
    shuffle_opts <- function(x) x[sample.int(length(x))]

    
    # Definice AI otázek
    
    sd_question(
      id = question_id[1],
      type = "mc",
      label = "**Která z následujících možností NENÍ typem umělé inteligence?**",
      option = shuffle_opts(c(
        "Strojové učení" = "A",
        "Zpracování přirozeného jazyka" = "B",
        "Blockchain" = "C",
        "Počítačové vidění" = "D"
      ))
    )
    
    sd_question(
      id = question_id[2],
      type = "mc",
      label = "**Jak se systémy umělé inteligence rozhodují?**",
      option = shuffle_opts(c(
        "Na základě matematicko-logických principů" = "A",
        "Na základě druhu programovacího jazyka" = "B",
        "Na základě kvantového provázání"  = "C",
        "Na základě umělé intuice" = "D"
      ))
    )
    
    sd_question(
      id = question_id[3],
      type = "mc",
      label = "**Četl/a jsi článek o tom, že AI dokáže samostatně řídit auta. Co to nejspíš umožňuje?**",
      option = shuffle_opts(c(
        "Umělá inteligence si předem zapamatuje všechny možné scénáře jízdy." = "A",
        "Umělá inteligence je naprogramována s pravidly if-else pro každou dopravní situaci." = "B",
        "Umělá inteligence se učí na základě údajů ze senzorů, kamer a předchozích zkušeností."  = "C",
        "Umělá inteligence se řídí instinkty a emocemi řidiče." = "D"
      ))
    )
    
    sd_question(
      id = question_id[4],
      type = "mc",
      label = "**Používáš chatbota s umělou inteligencí a zdá se, že rozumí Tvým otázkám a odpovídá přirozeně. Jaký je nejpravděpodobnější důvod, proč to AI dokáže?**",
      option = shuffle_opts(c(
        "Chatbot je naprogramován s předem napsanými odpověďmi na všechny možné otázky." = "A",
        "Chatbot používá zpracování přirozeného jazyka k analýze a generování textu podobného lidskému." = "B",
        "Chatbot má emoce a myslí jako člověk, proto dokáže odpovídat jako člověk."  = "C",
        "Chatbot dokáže přirozeně odpovědět pouze tehdy, pokud ve své databázi najde přesnou shodu." = "D"
      ))
    )
    
    sd_question(
      id = question_id[5],
      type = "mc",
      label = "**Jsi burzovní makléř/ka a slyšíš o umělé inteligenci, která dokáže předpovídat ceny akcií se 100% přesností. Co bys měl/a dělat?**",
      option = shuffle_opts(c(
        "Důvěřovat AI, protože její 100% přesnost naznačuje, že je hodně spolehlivá." = "A",
        "Být skeptický/á, protože žádná AI nedokáže dokonale předpovídat budoucnost." = "B",
        "Spoléhat se na tyto AI předpovědi, pokud se shodují s historickými trendy."  = "C",
        "Předpokládat, že AI dokáže předpovědět i náhodné události, jako jsou výsledky loterie, a podle toho jednat." = "D"
      ))
    )
    
    sd_question(
      id = question_id[6],
      type = "mc",
      label = "**Která z těchto možností je v současnosti běžným využitím umělé inteligence ve zdravotnictví?**",
      option = shuffle_opts(c(
        "Využití umělé inteligence ve zdravotnictví není v současnosti z legislativního hlediska možné." = "A",
        "Analýza lékařských snímků umělou inteligencí, která pomáhá při diagnostice nemocí." = "B",
        "Využití umělé inteligence pro psaní receptů pacientům bez nutnosti zapojení lékaře."  = "C",
        "Umělá inteligence se již používá pro nahrazení všech činností běžného lidského lékaře." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[7],
      type = "mc",
      label = "**Která z následujících možností NENÍ příkladem umělé inteligence v každodenním životě?**",
      option = shuffle_opts(c(
        "Spamové filtry v elektronické poště" = "A",
        "Online doporučovací systémy" = "B",
        "Automatické překladatelské služby" = "C",
        "Vědecká kalkulačka" = "D"
      ))
    )
    
    sd_question(
      id = question_id[8],
      type = "mc",
      label = "**Jaká je výhoda využití umělé inteligence při rozhodování v podnikání?**",
      option = shuffle_opts(c(
        "Umělá inteligence dokáže vždy učinit nejlepší rozhodnutí bez lidského dohledu." = "A",
        "AI odstraňuje potřebu zaměstnanců ve všech pracovních odvětvích." = "B",
        "AI dokáže rychle zpracovávat velké množství dat a rychle generovat důležitá zjištění." = "C",
        "AI může nahradit lidskou kreativitu, která je při rozhodování v podnikání důležitá." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[9],
      type = "mc",
      label = "**Jsi učitel/ka, který/á používá umělou inteligenci ke známkování esejů. Zjistil/a jsi, že AI má tendenci dávat nižší známky studentům s kratšími texty. Co bys měl/a udělat?**",
      option = shuffle_opts(c(
        "Nahraných dat bylo asi málo. Nahrát do systému mnohem více esejů a další informace o studentech." = "A",
        "Vložit do systému AI tréninková data esejů s různými délkami textů." = "B",
        "Říct studentům, aby psali delší texty." = "C",
        "Předpokládat, že AI je správná, protože je objektivnější než lidé." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[10],
      type = "mc",
      label = "**Používáš systém rozpoznávání obrazu s AI, který má potíže s rozpoznáváním tmavě zbarvených objektů. V čem by nejspíš mohl být problém?**",
      option = shuffle_opts(c(
        "AI nemá dobré noční vidění, a proto v noci dobře nepracuje." = "A",
        "V trénovacích datech AI chyběly obrázky tmavě zbarvených objektů." = "B",
        "AI obecně nedokáže dobře rozpoznávat objekty." = "C",
        "Systém AI je porouchaný a je nutné ho opravit." = "D"
      ))
    )
    
    sd_question(
      id = question_id[11],
      type = "mc",
      label = "**Co je to 'zkreslení' v modelech umělé inteligence?**",
      option = shuffle_opts(c(
        "Systémy AI, které byly navrženy programátory, kteří měli zkreslené názory." = "A",
        "Systematické chyby v rozhodování AI způsobené zkreslenými tréninkovými daty." = "B",
        "Systémy AI, které mají osobní preference." = "C",
        "Systémy AI, které dokáží zpracovávat pouze číselná data." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[12],
      type = "mc",
      label = "**Která z následujících možností je příkladem tzv. vysvětlitelné AI (explainable AI)?**",
      option = shuffle_opts(c(
        "Model AI, který umožňuje porozumět tomu, jak generuje své výstupy." = "A",
        "Model AI typu 'černá skříňka', který neprozrazuje, jakým způsobem činí rozhodnutí." = "B",
        "Modely AI, které se automaticky aktualizují bez lidského vstupu." = "C",
        "Obsah generovaný AI, který je k nerozeznání od obsahu vytvořeného člověkem." = "D"
      ))
    )
    
    sd_question(
      id = question_id[13],
      type = "mc",
      label = "**Která z následujících možností nejlépe popisuje problém tzv. černé skříňky v umělé inteligenci?**",
      option = shuffle_opts(c(
        "Modely AI jsou příliš jednoduché na interpretaci." = "A",
        "Rozhodovací proces je u AI obtížné pozorovat." = "B",
        "Systémy AI jsou vždy navrženy tak, aby byly zcela transparentní." = "C",
        "Modely AI je třeba před nasazením umístit do fyzické černé skříňky." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[14],
      type = "mc",
      label = "**Vyvíjíš AI, která pomáhá lékařům s určováním diagnóz. Zjistíš, že často chybně diagnostikuje vzácná onemocnění. Co bys měl/a nejdříve prověřit?**",
      option = shuffle_opts(c(
        "Zda lékaři souhlasí s diagnózami AI." = "A",
        "Zda AI systém zpracovává data příliš rychle a nedostatečně podrobně." = "B",
        "Zda tréninková data AI obsahovala vzácná onemocnění." = "C",
        "Zda AI potřebuje větší výpočetní výkon, protože vzácná onemocnění jsou složitější na diagnostiku." = "D"
      ))
    )
    
    sd_question(
      id = question_id[15],
      type = "mc",
      label = "**Společnost testuje AI, která doporučuje uchazeče o zaměstnání. Všimneš si, že většinou vybírá muže místo žen. Co může být příčinou?**",
      option = shuffle_opts(c(
        "Testovali ji jenom muži." = "A",
        "AI byla trénována na základě neobjektivních údajů o náboru, které upřednostňují muže." = "B",
        "AI nemůže být zaujatá, takže kandidáti museli být vybráni na základě objektivních údajů." = "C",
        "AI vybírá kandidáty náhodně, přičemž v těch několika případech použití náhodně vybrala muže, ale mohly to být i ženy." = "D"
      ))
    )
    
    
    sd_question(
      id = question_id[16],
      type = "mc",
      label = "**Co z níže uvedeného NENÍ etický problém týkající se tzv. deepfakes generovaných umělou inteligencí?**",
      option = shuffle_opts(c(
        "Deepfakes mohou být použity k zesměšňování či vydírání reálných osob." = "A",
        "Deepfakes mohou být použity k šíření dezinformací a manipulaci s veřejným míněním." = "B",
        "Deepfakes mohou být použity při tvorbě speciálních efektů v kinematografii." = "C",
        "Využití deepfakes může vést k narušení soukromí či neoprávněnému použití cizí identity." = "D"
      ))
    )
    
    sd_question(
      id = question_id[17],
      type = "mc",
      label = "**Který z následujících dopadů na životy lidí NEPŘEDSTAVUJE etický problém související s plošným zavedením systémů umělé inteligence?**",
      option = shuffle_opts(c(
        "Plošné zavedení systémů AI by vedlo k masovému sběru dat, což by mělo za následek narušení soukromí." = "A",
        "Plošné využívání AI by vedlo k výraznému zvýšení efektivity práce a automatizace rutinních pracovních úkolů." = "B",
        "Plošné zavedení systémů AI by vedlo k prohloubení sociálních nerovností v důsledku automatizace a nahrazování lidské práce AI." = "C",
        "Plošné využívání AI by bylo extrémně energeticky náročné, což by mělo negativní dopad na životní prostředí." = "D"
      ))
    )
    
    sd_question(
      id = question_id[18],
      type = "mc",
      label = "**Banka používá ke schvalování půjček systém umělé inteligence. Jednoho dne analytici v bance zjistí, že systém AI odmítá většinu žádostí z určité čtvrti. Co by měla banka udělat?**",
      option = shuffle_opts(c(
        "Zjistit, zda umělá inteligence nebyla trénována na zkreslených datech." = "A",
        "Předpokládat, že AI je spravedlivá, protože je založena na datech." = "B",
        "Použít AI pouze pro bohatší klienty." = "C",
        "Ignorovat tento problém, protože AI nemůže být neobjektivní." = "D"
      ))
    )
    
    sd_question(
      id = question_id[19],
      type = "mc",
      label = "**Firma používá AI k analýze vzájemné komunikace mezi zaměstnanci za účelem zvýšení produktivity. Jedná se o etický problém?**",
      option = shuffle_opts(c(
        "Ne, protože použití AI zvyšuje efektivitu, což převáží případné etické problémy." = "A",
        "Ano, protože může docházet k narušování soukromí zaměstnanců." = "B",
        "Ne, protože takové využití AI nezpůsobuje zaměstnancům žádnou přímou újmu." = "C",
        "Ano, ale jen v případě, že o tom zaměstnanci neví." = "D"
      ))
    )
    
    sd_question(
      id = question_id[20],
      type = "mc",
      label = "**Jedna z evropských zemí uvažuje o zavedení národního systému, který bude za využití AI rozhodovat o trestech odnětí svobody na základě předchozích případů. Co by s velkou pravděpodobností mohlo nastat v důsledku zavedení takového systému?**",
      option = shuffle_opts(c(
        "AI systém by nekriticky přejímal historické nespravedlnosti a prohluboval systémovou diskriminaci." = "A",
        "AI systém by přinesl stoprocentně objektivní a spravedlivé rozhodování, jelikož by nebyl založen na lidských předsudcích." = "B",
        "AI systém by zvýšil transparentnost soudních rozhodnutí díky objektivnímu algoritmu, který je založen na hlubokých neuronových sítích." = "C",
        "AI systém by nahradil všechny soudce a právníky, čímž by se významně snížily náklady na justiční systém." = "D"
      ))
    )
    
    sd_question(
      id = question_id[21],
      type = "mc",
      label = "**Student se zeptá asistenta AI: „Popiš největší vynálezce v historii.“ AI poskytne seznam vynálezců na základě historických záznamů nalezených online. Student některá jména v seznamu poznává a existenci ostatních si ověřil. Co by měl student udělat dál?**",
      option = shuffle_opts(c(
        "Zeptat se AI, zda existují vynálezci z nedostatečně zastoupených skupin, kteří také významně přispěli." = "A",
        "Předpokládat, že seznam AI je správný, protože vychází z historických záznamů." = "B",
        "Přijmout odpověď AI, protože obsahuje známé vynálezce." = "C",
        "Důvěřovat, že AI je neutrální a neupřednostňuje žádnou konkrétní skupinu." = "D"
      ))
    )
    
    sd_question(
      id = question_id[22],
      type = "mc",
      label = "**Student se zeptá asistenta AI: „Je cvičení v posilovně zdravé?“ AI odpoví ano a uvede následující důvody: zlepšená fyzická kondice, kontrola hmotnosti a přínosy pro duševní zdraví. Student však má podezření, že odpověď může být neúplná. Jaký je nejlepší další krok k získání vyváženější odpovědi?**",
      option = shuffle_opts(c(
        "Zeptat se: „Existují nějaké možné nevýhody nebo rizika cvičení v posilovně?“" = "A",
        "Přeformulovat otázku na: „Řekni mi více o tom, proč je cvičení v posilovně prospěšné.“" = "B",
        "Předpokládat, že AI má pravdu, protože posilovny jsou navrženy tak, aby podporovaly zdraví." = "C",
        "Položit stejnou otázku znovu a zjistit, zda AI poskytne jinou odpověď." = "D"
      ))
    )
    
    sd_question(
      id = question_id[23],
      type = "mc",
      label = "**Po položení otázky ke zlepšení akademického výkonu AI poskytlo odpověď: „Vstávání brzy ráno je nejlepší způsob, jak si studenti mohou zlepšit akademický výkon.“ Jak nejlépe při interakci s AI toto tvrzení kriticky ověřit?**",
      option = shuffle_opts(c(
        "Zeptat se AI, zda zohledňuje všechny relevantní faktory (např. délka spánku, kvalita spánku apod.)." = "A",
        "Přimět AI, ať uvede důvody, proč je brzké vstávání pro lidské tělo naopak špatné." = "B",
        "Zeptat se AI, zda existují lidé, kteří dosáhli velkého úspěchu a zároveň vstávali brzy." = "C",
        "Předpokládat, že AI má pravdu, protože zpracovala velké množství dat a má přístup k vědeckým článkům." = "D"
      ))
    )
    
    sd_question(
      id = question_id[24],
      type = "mc",
      label = "**Článek o zdraví generovaný AI uvádí: „Voda je nezbytná pro život člověka na Zemi.“ Student chce toto tvrzení kriticky zpochybnit. Jaký je nejlepší přístup?**",
      option = shuffle_opts(c(
        "Přijmout tvrzení, protože je široce podpořeno vědeckým výzkumem a lidskou biologií." = "A",
        "Předpokládat, že AI může přehánět, a proto zkusit na internetu vyhledat studie, které dokazují, že voda není nezbytná." = "B",
        "Zpochybnit, zda je pitná voda skutečně nezbytná, protože AI může být někdy zavádějící." = "C",
        "Odmítnout tvrzení a požádat AI o argumenty proti pití vody." = "D"
      ))
    )
    
    sd_question(
      id = question_id[25],
      type = "mc",
      label = "**Studenta zajímají informační technologie, a proto by se chtěl této oblasti v budoucnosti věnovat. Požádá AI asistenta o radu při výběru kariérní cesty, ale odpověď je příliš obecná a nepomůže mu. Jaký je nejlepší další krok k získání užitečnější informace?**",
      option = shuffle_opts(c(
        "Upravit prompt přidáním omezení, například: „Ukaž mi možnosti kariéry v oblasti informačních technologií na základě mého zájmu o počítačovou grafiku.“" = "A",
        "Přeformulovat prompt, aby byl ještě otevřenější, například: „Pověz mi něco o kariérních cestách.“" = "B",
        "Upravit prompt složitějšími požadavky, například: „Vyjmenuj všechny možné profese v technických profesích, včetně detailů o platu, dostupnosti pracovních míst a požadovaných dovednostech u každé z nich.“" = "C",
        "Zúžit prompt tím, že se zeptá: „Jaké jsou nejlépe placené pracovní pozice v oblasti informačních technologií, které mohu získat bez vysokoškolského titulu?“" = "D"
      ))
    )
    
    sd_question(
      id = question_id[26],
      type = "mc",
      label = "**Student/ka požádá AI, aby shrnula román pro knižní recenzi, který také sám/sama četl/a. Student/ka si všimne, že odpověď AI je vágní a chybí v ní klíčové dějové body. Nemá však čas napsat svou recenzi kompletně sám/a. Jak by měl student/ka přizpůsobit svůj přístup?**",
      option = shuffle_opts(c(
        "Opakovaně upravovat prompt tak, že se specifikují klíčové dějové body, důležitá témata, vývoj postav apod." = "A",
        "Požádat AI, aby shrnula příběh mnohem stručněji a předpokládat, že odstranění detailů zlepší jasnost." = "B",
        "Přeformulovat prompt tak, aby se soustředil pouze na vybrané detaily." = "C",
        "Požádat o shrnutí, ale nařídit AI použít sofistikovaný akademický jazyk a předpokládat, že to učiní odpověď přesnější." = "D"
      ))
    )
    
    sd_question(
      id = question_id[27],
      type = "mc",
      label = "**Student použije asistenta AI k výzkumu historické události, ale odpověď obsahuje fakt, který je v rozporu s jejich učebnicí. Jak by měl student vyhodnotit informace generované AI?**",
      option = shuffle_opts(c(
        "Ověřit tvrzení dalšími zdroji, než se rozhodne, zda mu důvěřovat." = "A",
        "Předpokládat, že tvrzení AI je správné, protože je založené na obrovském množství dat." = "B",
        "Upozornit AI na tuto chybu a předpokládat, že AI už tuto chybu nikdy neudělá." = "C",
        "Odmítnout učebnici jako zastaralou a důvěřovat AI, protože poskytuje novější informace." = "D"
      ))
    )
    
    sd_question(
      id = question_id[28],
      type = "mc",
      label = "**Společnost používá nástroj poháněný AI k hodnocení uchazečů o práci, ale všechny jeho nejlepší doporučení jsou muži středního věku. Měla by společnost zvažovat výstupy tohoto nástroje AI při rozhodování o náboru?**",
      option = shuffle_opts(c(
        "Ano, nicméně AI může odrážet předsudky v datech, na kterých byla trénována, což může vést k neférovým výsledkům náboru, takže její doporučení by měla být pečlivě přezkoumána." = "A",
        "Ano, protože AI je neutrální a činí objektivní rozhodnutí, zatímco náboroví specialisté (lidé) mohou udělat lidské chyby a ovlivnit doporučení." = "B",
        "Ne, protože doporučování pouze mužů jako uchazečů o práci zavádí předsudky a jakýkoli nástroj s předsudky by neměl být nikdy použit." = "C",
        "Ano, protože AI analyzovala mnohem více žádostí než jakýkoli náborový specialista (člověk) během jeho/její kariéry. Společnost by měla předpokládat, že doporučení AI jsou správná." = "D"
      ))
    )
    
    sd_question(
      id = question_id[29],
      type = "mc",
      label = "**Student vidí na sociálních sítích video, ve kterém známý politik říká velmi kontroverzní výrok. Video působí realisticky, má vysoké rozlišení a tisíce sdílení. Student se zamýšlí, zda je video autentické, nebo zda mohlo být vytvořeno pomocí generativní AI. Jaký přístup je nejvhodnější?**",
      option = shuffle_opts(c(
        "Spoléhat na vizuální kvalitu a realističnost videa – kdyby bylo falešné, vypadalo by méně přesvědčivě." = "A",
        "Ověřit pravost videa pomocí fact-checkingových webů nebo ověřovacích nástrojů – deepfakes lze obvykle odhalit při pečlivé analýze." = "B",
        "Předpokládat, že video je pravdivé, protože bylo zveřejněno známým účtem a dosud nebylo vyvráceno." = "C",
        "Připustit, že v některých případech není možné s jistotou určit, zda je video pravé nebo generované AI, a proto je nutné k jeho obsahu přistupovat s kritickým odstupem a hledat další ověřené informace." = "D"
      ))
    )
    
  })
  
  anon_code <- sd_reactive("anon_code", {
    paste0(input$anon_m1, input$anon_m2, input$anon_t1, input$anon_t2)
  })
  
  # Block "Next" unless all four letters are picked
  sd_stop_if(
    any(vapply(list(input$anon_m1, input$anon_m2, input$anon_t1, input$anon_t2),
               function(x) is.null(x) || identical(x, ""), logical(1))) ~
      "Vyber, prosím, všechna čtyři písmena pro anonymizační kód."
  )
  
  # If the consent answer is "Nesouhlasím", jump straight to the 'zaver' page
  sd_skip_if(
    !isTRUE(input$info_souhlas_1 == "souhlasim") ~ "zaver"
  )
  
  # helper: treat NULL as character(0)
  or0 <- function(x) if (is.null(x)) character(0) else x
  
  sd_show_if(
    isTRUE("jine" %in% or0(input$gender)) ~ "gender_jine_text",
    isTRUE("jina" %in% or0(input$narodnost)) ~ "narodnost_jina_text",
    isTRUE("jina" %in% or0(input$funkce)) ~ "funkce_jina_text",
    isTRUE("cizi_jazyk" %in% or0(input$kvalifikace))  ~ "kvalifikace_cizi_jazyk_text",
    isTRUE("odborne_predmety" %in% or0(input$kvalifikace))  ~ "kvalifikace_odborne_predmety_text",
    isTRUE("jiny_predmet" %in% or0(input$kvalifikace))  ~ "kvalifikace_jiny_predmet_text"
  )
    
  sd_server(db = db,
            system_language = "cs"
  )

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
