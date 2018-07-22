$(() => {
  const videoIds = [
    "iNeLpmjowwQ",
    "cDYn_la-9vg",
    "hp-uQZ-MujA",
    "uB7ES5-JhQA",
    "vcE9BlW74tg",
    "sP6Iuwd_Nik"
  ];

  const divContainer = $("#carousel .carousel-inner");
  const divTemplate = $("#carousel-template");

  for (let i = 0; i < videoIds.length; ++i) {
    const div = $(divTemplate.html());
    div.find("> :first-child").attr("src", `https://www.youtube.com/embed/${videoIds[i]}?rel=0`);

    if (i == 0) {
      div.addClass("active");
    }

    divContainer.append(div);
  }
});
