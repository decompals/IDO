#include <dmedia/midi.h>

class MFfile;

#if 0
struct mfEvent {
    MDevent ev;
    int track;
};
#endif
struct MDevent_list {
    MDevent data;
    MDevent_list *next, *prev;
};

class MFtrack {
  public:
    MFtrack(MFfile *);
    void insertevent(MDevent *);
    void deleteevent();
    void seektime(unsigned long long);
    MDevent *seekevent(MDevent *);
    MDevent *nextevent(void);
    MDevent *prevevent(void);
    MDevent *currentevent(void);
    unsigned long long firststamp(void);
    int Howmany(void);
    void rewind(void);
  private:
    MFfile *parent_;
    MDevent_list *data_;
    MDevent_list *current_;
    int howmany_;
};
