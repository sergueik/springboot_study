package cs305.minesweeper.model;

import java.util.LinkedList;
import java.util.Random;

import cs305.minesweeper.MinesweeperBoard;
import cs305.minesweeper.MinesweeperTile;
import cs305.minesweeper.MinesweeperBoard.GameState;
import cs305.minesweeper.MinesweeperTile.TileState;

/**
 * Logic for the minesweeper Project.
 * @author Larry
 *
 */
public class LogicModel {
	private MinesweeperBoard board;


	
	public MinesweeperBoard getBoard(){
		return board;
	}
	
	public void setBoard(MinesweeperBoard mb){
		this.board = mb;
	}

	/**
	 * "Main Method" of LogicModel. This is what plays the game.
	 * 
	 * @return true if the LogicModel wins, false if it loses.
	 */
	public boolean play(){
		if (board == null){
			System.out.println("Please pass a board to the LogicModel");
			return false;
		}
		
		LinkedList<MinesweeperTile> stackOfTiles = new LinkedList<MinesweeperTile>();
		stackOfTiles.add(guessTile());
		int lastTryAmmountOfUnknown = stackOfTiles.size(); // sentinel value

		while (board.getGameState() == GameState.Playing){
			LinkedList<MinesweeperTile> potentiallyActedUponTiles = actOnTile(stackOfTiles.pollLast());

			if(potentiallyActedUponTiles != null) //Stuff has been clicked
				stackOfTiles.addAll(potentiallyActedUponTiles);
			else //stuff has not been clicked
				if(!stackOfTiles.isEmpty()){
					;//do nothing loop again
				}else{//stack is empty, we should try again.
					int newLastTryAmmountOfUnknown = addNumberTiles(stackOfTiles);
					if (lastTryAmmountOfUnknown == (stackOfTiles.size())){
							stackOfTiles.add(guessTile());
					}
					lastTryAmmountOfUnknown = newLastTryAmmountOfUnknown;
				}
		}
		if (board.getGameState() == GameState.Won){
			return true; 
		}
		return false;
	}

	private int addNumberTiles(LinkedList<MinesweeperTile> stack) {
		MinesweeperTile[] tiles = board.getNumberTiles();
		for (MinesweeperTile tile : tiles){
			stack.add(tile);
		}
		return tiles.length;
	}

	/**
	 * Guesses a random tile when the algorithm is "stuck".
	 * @return The tile clicked on.
	 */
	private MinesweeperTile guessTile() {

		MinesweeperTile[] tiles = board.getHiddenTiles();
		Random r = new Random();
		int random = r.nextInt(tiles.length);
		board.clickTile(tiles[random]);
		return tiles[random];
	}

	private LinkedList<MinesweeperTile> actOnTile(MinesweeperTile t){
		TileInfo tile = new TileInfo(t);
		if (tile.numSurroundingHidden != 0 && 
				tile.value !=0 &&
				tile.tile.getState() != TileState.Hidden){
			if (canFlagSurroundingTiles(tile))
				return flagSurroundingUnknownTiles(tile.surroundingHiddenTiles);
			else 
				if (canClickSurroundingTiles(tile))
					return clickSurroundingUnknownTiles(tile.surroundingHiddenTiles);
		}
		return null;
	}

	/**
	 * A bomb(s) needs to be flagged if the number of unknown and flagged tiles
	 * is less than the tile's number.
	 * 
	 * This method checks that case, and returns true if all surrounding unknown tiles can 
	 * be flagged or false if they can not.
	 * 
	 * Maybe Pass back a collection of unknowns for true, and null for false.
	 * 
	 * @return
	 */
	private boolean canFlagSurroundingTiles(TileInfo tile){
		if (tile.numSurroundingHidden + tile.numSurroundingFlagged <= tile.value)
			return true;
		return false;
	}

	/**
	 * Surrounding tiles can be clicked if surrounding unknown is less then the surrounding
	 * flagged tiles and this tiles value.
	 * 
	 * This method checks that case, and returns true if all surrounding unknown tiles can 
	 * be clicked or false if they can not.
	 * 
	 * 
	 * @param t: The tile whose surroundings we are looking at.
	 * @return
	 */
	private boolean canClickSurroundingTiles(TileInfo t){
		if (t.value == t.numSurroundingFlagged)
			return true;
		return false;
	}
	/**
	 * Returns the 
	 * @param hiddenTiles
	 */
	private LinkedList<MinesweeperTile> clickSurroundingUnknownTiles(LinkedList<MinesweeperTile> hiddenTiles){
		LinkedList<MinesweeperTile> newlyClickedTiles = new LinkedList<MinesweeperTile>();
		for (MinesweeperTile t : hiddenTiles){
			if(t.getState() != TileState.Exposed){
				this.board.clickTile(t);
				newlyClickedTiles.add(t);
			}
		}

		return newlyClickedTiles;
	}

	private LinkedList<MinesweeperTile> flagSurroundingUnknownTiles(LinkedList<MinesweeperTile> hiddenTiles){
		LinkedList<MinesweeperTile> newlyClickedTiles = new LinkedList<MinesweeperTile>();
		for (MinesweeperTile t : hiddenTiles){
			if(t.getState() != TileState.Exposed){
				this.board.flagTile(t);
				newlyClickedTiles.add(t);
			}
		}

		int tilesClicked = newlyClickedTiles.size();
		for(int i = 0; i < tilesClicked; i++){
			TileInfo t = new TileInfo(newlyClickedTiles.get(i));
			newlyClickedTiles.addAll(t.surroundingNumberTiles);
		}

		return newlyClickedTiles;
	}

	/**
	 * TileInfo's purpose is to hold the info on a tile.  well as 
	 * recalculate what the tile knows about it's surroundings
	 * @author Larry
	 */
	private class TileInfo{
		/**
		 * The number of surrounding tiles that are flagged.
		 */
		int numSurroundingFlagged;

		/**
		 * The number of surrounding tiles that are unknown.
		 */
		int numSurroundingHidden;

		/**
		 * This tile's number. 
		 */
		int value;

		/**
		 * This is the tile's whose info is contained in this class.
		 */
		MinesweeperTile tile;

		/**
		 * Holds the surrounding number tiles.
		 */
		LinkedList<MinesweeperTile> surroundingHiddenTiles;

		LinkedList<MinesweeperTile> surroundingNumberTiles;

		/**
		 * Constructor for the TileInfo class. Must be passed a non-null tile.
		 * @param t: The tile whose info will be kept in this class
		 */
		public TileInfo(MinesweeperTile t){
			this.tile = t;
			if(tile.getState() == TileState.Flagged)
				this.value = 0;
			else
				this.value = t.getValue();
			parseTileSurroundings();
		}


		/**
		 * Calculates how many flagged and unknown tiles are surrounding
		 * this tile.
		 */
		public void parseTileSurroundings(){
			if(surroundingHiddenTiles == null){
				surroundingHiddenTiles = new LinkedList<MinesweeperTile>();
			}
			if (surroundingNumberTiles == null){
				surroundingNumberTiles = new LinkedList<MinesweeperTile>();
			}
			surroundingHiddenTiles.clear();
			surroundingNumberTiles.clear();

			MinesweeperTile[] surroundingTiles = tile.getSurroundingTiles();
			int surroundingFlagged = 0;
			int surroundingHidden = 0;
			for(MinesweeperTile tempTile : surroundingTiles)
				if (tempTile.getState() == TileState.Flagged)
					surroundingFlagged++;
				else if (tempTile.getState() == TileState.Hidden){
					surroundingHidden++;
					surroundingHiddenTiles.add(tempTile);
				}
				else if(tempTile.getValue() > 0 && tempTile.getValue() < 10)
					surroundingNumberTiles.add(tempTile);
			this.numSurroundingFlagged = surroundingFlagged;
			this.numSurroundingHidden = surroundingHidden;
		}
	}
}
