$(() => {
  const videoIds = [
    "iNeLpmjowwQ",
    "cDYn_la-9vg",
    "hp-uQZ-MujA"
  ];

  const divContainer = $("#carousel .carousel-inner");
  const divTemplate = $("#carousel-template");

  for (let i = 0; i < videoIds.length; ++i) {
    const div = $(divTemplate.html());
    div.find("> :first-child").attr("src", `https://www.youtube.com/embed/${videoIds[i]}`);

    if (i == 0) {
      div.addClass("active");
    }

    divContainer.append(div);
  }
});
