#include <miditrack.h>
#include <midifile.h>
#include <stdlib.h>
#include <memory.h>
#include <values.h>

MFtrack::MFtrack(MFfile *parent)
{
    parent_ = parent;
    data_ = 0;
    current_ = 0;
    howmany_=0;
}

void
MFtrack::rewind()
{
    current_ = data_;
}

void
MFtrack::insertevent(MDevent *ev)
{
    MDevent_list *tmp;
    tmp = new(MDevent_list);
    tmp->data = *ev;
    if (ev->msglen > 3) {
	    tmp->data.sysexmsg = (char *) malloc(ev->msglen);
	    memcpy(tmp->data.sysexmsg, ev->sysexmsg, ev->msglen);	
    }

    if (!current_) {
	current_ = data_ = tmp;
	current_->next = 0;
	current_->prev = 0;
    } else if (!(current_->next)) { // appending and advance
	current_->next = tmp;
	tmp->prev = current_;
	tmp->next = 0;
	current_ = tmp;
    } else {			// inserting into middle
	MDevent_list *foo;
	foo = current_->next;
	current_->next = tmp;
	tmp->prev = current_; 
	tmp->next = foo;
	foo->prev = tmp;
	current_ = tmp;
    }
}

void
MFtrack::deleteevent()
{
}

void
MFtrack::seektime(unsigned long long time)
{
	if (time == 0) {
		current_ = data_;
		return;
	}
	if (!current_) current_ = data_;
	if (!current_) return;
	if (time > current_->data.stamp) {
		while (time >= current_->data.stamp) {
			if (current_->next) {
				current_ = current_->next;
		        } else {
				break;
			}
		}
		
	} else {
		while (time < current_->data.stamp) {
			if (current_->prev) {
				current_ = current_->prev;
			} else {
				break;
			}
		}

		if (current_->next)
			current_ = current_->next;
	}
}

MDevent *
MFtrack::seekevent(MDevent* /*event*/)
{
    return 0;
}

MDevent *
MFtrack::nextevent()
{
    MDevent *retrn = currentevent();

    if (current_) {
	current_ = current_->next;
    } else
	return 0;

    return retrn;
}

MDevent *
MFtrack::currentevent()
{
	howmany_ = 0;
	if (!current_) return &(data_->data);

	return &(current_->data);
}

MDevent *
MFtrack::prevevent()
{
    if (current_) {
	current_ = current_->prev;
    } 

    return currentevent();
}

// firststamp: return the timestamp of the first
// available event on the track, and also how many
// events have the same stamp.

unsigned long long
MFtrack::firststamp(void)
{
	MDevent_list *tmp = current_;
	unsigned long long retrn;
	if (current_) 
		retrn = current_->data.stamp;
	else
		return MAXINT;

	if (!howmany_) {
		while (tmp->data.stamp == current_->data.stamp) {
			howmany_++;
			if (tmp->next)
				tmp = tmp->next;
			else
				break;
		}
	}
	return retrn;
}

int
MFtrack::Howmany(void)
{
	return howmany_;
}
