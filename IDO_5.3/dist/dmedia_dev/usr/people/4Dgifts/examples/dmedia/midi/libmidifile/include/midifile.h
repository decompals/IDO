#include <dmedia/midi.h>
#include <stdio.h>
#include <sys/types.h>

class MFfile;
class MFtrack;

enum MIDImetaEvents {
    MIDImeta_SeqNumber		= 0x00,
    MIDImeta_Text		= 0x01,
    MIDImeta_Copyright		= 0x02,
    MIDImeta_Name		= 0x03,
    MIDImeta_Instrument		= 0x04,
    MIDImeta_Lyric		= 0x05,
    MIDImeta_Marker		= 0x06,
    MIDImeta_CuePoint		= 0x07,

    MIDImeta_ChannelPrefix	= 0x20,
    MIDImeta_EOT		= 0x2F,
    MIDImeta_SetTempo		= 0x51,
    MIDImeta_SMPTEoffset	= 0x54,
    MIDImeta_TimeSignature	= 0x58,
    MIDImeta_KeySignature	= 0x59
};

class MFfile {
  public: 
    MFfile();
    ~MFfile();
    void seek(unsigned long long time);
    unsigned long long tell();
    int ntracks() { return ntracks_; }
    unsigned long long length();
    MFtrack *gettrack(int n);
    void addtrack(MFtrack *);
    void deletetrack(MFtrack *);
    void tempo(int);
    int gettempo(void);
    void settempo(int t) {tempo_ = t;}
    int open(char *name, char *mode);
    int create(char *name);
    int read();
    int readtrack(MFtrack **);
    int newtrack();
    void deletetrack(int);
    int save();
    int savetrack(MFtrack *);
    void ticks2msec(MDevent *);
    int msec2ticks(int);
    void rewind();
    int nextevent(MDevent *, int);
    int writetrack(int);        // write individual track
    void writefile();	        // write whole file
    int format();	        // format of this midi file 
    void setformat(int i) 
        {format_ = i;}
    int division() { return division_; }  // clock ticks per metronome
    void setdivision(int i) 
        {division_ = i;}
    int ppq();                  // MIDI clocks per tick(almost always 24?)
    void setppq(int i) 
        {ppq_ = i;}
    int numer();	        // numerator of time signature
    int denom();		// denominator of t.s.
    void settimesig(int n ,int d) 
        {numer_ = n; denom_ = d;}
    int thirt2s();		// # 32nd notes per quarter note
    void setthirt2s(int i) 
        {thirt2s_ = i;}
    int inited_;

  private:
    int tempo_;         // msecs per quarter note
    MFtrack **tracks_;  // array of ptrs to tracks, maybe a linked list.
    int ntracks_;      
    int toread_;	// number of bytes to read in current chunk 
    int format_;	// format of this midi file 
    int division_;	// clock ticks per metronome
    int ppq_;           // MIDI clocks per   " (almost always 24?)
    int numer_;		// numerator of time signature
    int denom_;		// denominator of t.s.
    int thirt2s_;	// # 32nd notes per quarter note
    long backtrack_;
    FILE *fp_;
    unsigned long long lasttime_;
    int readfilehdr();
    int readtrackhdr();
    int readchar();
    int read32();
    int read16();
    int readvar();
    int readhd(char *,int);
    void timesig(char,char,char,char);
    int writemeta(MIDImetaEvents, ...);
    void write32(int);
    void write16(int);
    int writevar(int);
    int writetime(int);
    int writeevent(MDevent *);
    int writehd(char *looking);
    void writefilehdr();
    void writetrackhdr();
    unsigned long long length_;
    void doinit();
};
