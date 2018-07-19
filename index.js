$(() => {
  const videoIds = [
    "iNeLpmjowwQ",
    "cDYn_la-9vg",
    "hp-uQZ-MujA"
  ];

  const liContainer = $("#carousel .carousel-indicators");
  const divContainer = $("#carousel .carousel-inner");
  const liTemplate = $("#video-li");
  const divTemplate = $("#video-div");

  for (let i = 0; i < videoIds.length; ++i) {
    const li = $(liTemplate.html());
    li.attr("data-slide-to", i.toString());

    const div = $(divTemplate.html());
    div.find(">:first-child").attr("src", `https://www.youtube.com/embed/${videoIds[i]}`);

    if (i == 0) {
      li.addClass("active");
      div.addClass("active");
    }

    liContainer.append(li);
    divContainer.append(div);
  }
});
