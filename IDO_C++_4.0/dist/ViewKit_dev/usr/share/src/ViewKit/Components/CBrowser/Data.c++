#include "Data.h"

extern VkComponent* createRepeatButton(char *, Widget);
extern VkComponent* createModified(char *, Widget);
extern VkComponent* createTextIO(char *, Widget);
extern VkComponent* createResizer(char *, Widget);
extern VkComponent* createScroll(char *, Widget);
extern VkComponent* createGraph(char *, Widget);
extern VkComponent* createPrefItem(char *, Widget);
extern VkComponent* createListSearch(char *, Widget);
extern VkComponent* createCompletionField(char *, Widget);
extern VkComponent* createCheckBox(char *, Widget);
extern VkComponent* createRadioBox(char *, Widget);
extern VkComponent* createDeck(char *, Widget);
extern VkComponent* createGangedGroup(char *, Widget);
extern VkComponent* createMeter(char *, Widget);
extern VkComponent* createPie(char *, Widget);
extern VkComponent* createOutline(char *, Widget);
extern VkComponent* createFileSet(char *, Widget);
extern VkComponent* createTabPanel(char *, Widget);
extern VkComponent* createVuMeter(char *, Widget);
extern VkComponent* createTickMarks(char *, Widget);

ComponentInfo componentInfo[] = {
  { "VkCheckBox", createCheckBox, False },
  { "VkCompletionField", createCompletionField, False },
  { "VkDeck", createDeck, False },
  { "VkFileSet", createFileSet, True },
  { "VkGangedGroup", createGangedGroup, False },
  { "VkGraph", createGraph, False },
  { "VkListSearch", createListSearch, False },
  { "VkMeter", createMeter, False },
  { "VkModifiedAttachment", createModified, False },
  { "VkPie", createPie, False },
  { "VkPrefItem", createPrefItem, False },
  { "VkRadioBox", createRadioBox, False },
  { "VkRepeatButton", createRepeatButton, False },
  { "VkResizer", createResizer, False },
  { "VkScroll", createScroll, False },
  { "VkTabPanel", createTabPanel, False },
  { "VkTickMarks", createTickMarks, False },
  { "VkTextIO", createTextIO, False },
  { "VkOutline", createOutline, False },
  { "VkVuMeter", createVuMeter, False },
};

int componentInfoSize = XtNumber(componentInfo);
